(ns clj-surgeon.move-into
  "Move top-level forms into an existing namespace file.

   Plan: docs/plans/move-into-existing-namespace.md. Everything here is pure:
   source texts in, complete future texts or a typed refusal out. The CLI
   shell in clj-surgeon.extract reads the files, writes the result
   atomically, lints it, and rolls back on a new error."
  (:require
   [clj-surgeon.analyze :as analyze]
   [clj-surgeon.cljc.require-ops :as require-ops]
   [clj-surgeon.extract :as extract]
   [clj-surgeon.extract-header :as header]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.move-callers :as move-callers :refer [ns-clause ns-zloc remove-libspecs]]
   [clojure.set :as set]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as parser]
   [rewrite-clj.zip :as z]))

;; ============================================================
;; Target model
;; ============================================================

(defn- token-string [zloc]
  (when zloc
    (if (= :meta (n/tag (z/node zloc)))
      (some-> zloc z/down z/rightmost z/string)
      (z/string zloc))))

(defn- comment-start
  "0-based index of the first line of the `;` comment block directly above
  the form that starts on 1-based `form-line`."
  [lines form-line]
  (loop [i (- form-line 2)]
    (cond
      (neg? i) 0
      (str/starts-with? (str/trim (nth lines i "")) ";") (recur (dec i))
      :else (inc i))))

(defn top-level-forms
  "Every top-level list form of `source`: its type, name, dispatch value (for
  defmethod), 1-based start and end rows, and the 0-based index of its first
  attached comment line."
  [source]
  (let [lines (vec (str/split-lines source))]
    (->> (z/of-string source {:track-position? true})
         (iterate z/right)
         (take-while some?)
         (filter z/list?)
         (mapv (fn [zloc]
                 (let [head (z/down zloc)
                       name-loc (some-> head z/right)
                       {:keys [row end-row]} (meta (z/node zloc))]
                   {:type (some-> head z/string)
                    :name (token-string name-loc)
                    :name-symbol? (and (some? name-loc)
                                       (symbol? (try (z/sexpr name-loc)
                                                     (catch Exception _ nil))))
                    :dispatch (some-> name-loc z/right z/string)
                    :line row
                    :end-line end-row
                    :comment-start (comment-start lines row)}))))))

(defn- defining? [project-aliases {:keys [type]}]
  (and type
       (not= "declare" type)
       (some? (forms/spec-with-project-aliases project-aliases type))))

(defn- typed-anchor [{:keys [type name dispatch]} same-name-count]
  (if (and (= "defmethod" type) dispatch)
    (str "defmethod " name " " dispatch)
    (if (> same-name-count 1) (str type " " name) name)))

;; ============================================================
;; Anchors
;; ============================================================

(def anchor-keys [:above :below :at-start :at-end])

(defn- parse-named-anchor
  "\"render\", \"defmulti render\", or \"defmethod render :card\"."
  [text]
  (let [parts (str/split (str/trim (str text)) #"\s+" 3)]
    (case (count parts)
      1 {:name (first parts)}
      2 {:type (first parts) :name (second parts)}
      {:type (first parts) :name (second parts) :dispatch (nth parts 2)})))

(defn resolve-anchor
  "Resolve exactly one of :above, :below, :at-start, :at-end against the
  target's definitions. Returns {:position :above|:below|:at-start|:at-end
  ...the anchored form...} or a refusal."
  [target anchor project-aliases]
  (let [given (filterv #(some? (get anchor %)) anchor-keys)]
    (if (not= 1 (count given))
      {:ok false
       :error-type :anchor-required
       :error "Name exactly one of --above FORM, --below FORM, --at-start, --at-end"
       :given (mapv #(str "--" (name %)) given)}
      (let [position (first given)]
        (if (#{:at-start :at-end} position)
          {:ok true :position position}
          (let [wanted (parse-named-anchor (get anchor position))
                all-forms (top-level-forms target)
                definitions (filterv #(defining? project-aliases %) all-forms)
                by-name (filterv #(= (:name wanted) (:name %)) definitions)
                matches (filterv #(and (or (nil? (:type wanted))
                                           (= (:type wanted) (:type %)))
                                       (or (nil? (:dispatch wanted))
                                           (= (:dispatch wanted) (:dispatch %))))
                                 by-name)]
            (case (count matches)
              1 (assoc (first matches) :ok true :position position)
              0 (let [unrecognized (some #(when (and (= (:name wanted) (:name %))
                                                     (:name-symbol? %)
                                                     (not (defining? project-aliases %)))
                                            %)
                                         all-forms)]
                  (cond->
                    {:ok false
                     :error-type :anchor-not-found
                     :error (str "The target defines no form named "
                                 (get anchor position))
                     :anchor (get anchor position)
                     :alternatives ["--at-start" "--at-end"]}
                    unrecognized
                    (assoc :unrecognized-macro (:type unrecognized)
                           :config-entry {:aliases {(:type unrecognized) {:kind :def}}}
                           :next-step (str "Declare " (:type unrecognized)
                                           " under :aliases in .clj-surgeon.edn so clj-surgeon "
                                           "recognizes it as a defining form, or anchor with "
                                           "--at-start or --at-end"))))
              {:ok false
               :error-type :ambiguous-anchor
               :error (str "The anchor " (get anchor position)
                           " matches " (count matches) " forms")
               :anchor (get anchor position)
               :matches (mapv (fn [form]
                                {:anchor (typed-anchor form (count matches))
                                 :type (:type form)
                                 :line (:line form)})
                              matches)})))))))

;; ============================================================
;; Placement
;; ============================================================

(defn- line-chunks
  "Split text into lines that keep their terminators."
  [text]
  (let [chunks (vec (str/split text #"(?<=\n)" -1))]
    (if (and (seq chunks) (= "" (peek chunks))) (pop chunks) chunks)))

(defn- with-final-newline [s]
  (if (or (str/blank? s) (str/ends-with? s "\n")) s (str s "\n")))

(defn place-block
  "Insert `block` (complete source of the moved forms, no surrounding blank
  lines) into `target` at the resolved anchor, one blank line on each side."
  [target anchor project-aliases block]
  (let [resolved (resolve-anchor target anchor project-aliases)]
    (if-not (:ok resolved)
      resolved
      (let [chunks (line-chunks target)
            ns-form (first (filter #(= "ns" (:type %)) (top-level-forms target)))
            split-at-line (fn [index]
                            [(apply str (subvec chunks 0 index))
                             (apply str (subvec chunks index))])
            after (fn [end-row]
                    (let [[prefix suffix] (split-at-line end-row)]
                      (str (with-final-newline prefix) "\n" block "\n"
                           (if (str/blank? suffix) "" suffix))))
            result
            (case (:position resolved)
              :below (after (:end-line resolved))
              :at-start (after (:end-line ns-form))
              :above (let [[prefix suffix] (split-at-line (:comment-start resolved))]
                       (str prefix block "\n\n" suffix))
              :at-end (str (with-final-newline (str/trimr target)) "\n" block "\n"))]
        {:ok true :target result :anchor resolved}))))

;; ============================================================
;; Requires and qualifier rewrites
;; ============================================================

(defn ns-form-source
  "The source text of the ns form in `text`, or nil."
  [text]
  (some->> (z/of-string text)
           (iterate z/right)
           (take-while some?)
           (filter #(and (z/list? %) (= "ns" (some-> % z/down z/string))))
           first
           z/string))

(defn- rewrite-token [text rewrites]
  (let [rewrite (fn [prefix local]
                  (when (contains? rewrites prefix)
                    (if-let [replacement (get rewrites prefix)]
                      (str replacement "/" local)
                      local)))]
    (if-let [[_ prefix local] (re-matches #"::([^/]+)/(.+)" text)]
      (some->> (rewrite prefix local) (str "::"))
      (when-let [[_ prefix local] (re-matches #"([^:/][^/]*)/(.+)" text)]
        (rewrite prefix local)))))

(defn rewrite-qualifiers
  "Rewrite qualified symbols and auto-resolved keywords in `source` whose
  qualifier is a key of `rewrites`: to `new/local` when the value is a
  string, to the plain local name when it is nil. Returns {:source :rewritten}."
  [source rewrites]
  (loop [loc (z/of-string source)
         rewritten []]
    (cond
      (z/end? loc)
      {:source (z/root-string loc) :rewritten rewritten}

      (= :token (n/tag (z/node loc)))
      (let [text (z/string loc)
            replacement (rewrite-token text rewrites)]
        (if replacement
          (recur (z/next (z/replace loc (parser/parse-string replacement)))
                 (conj rewritten {:from text :to replacement}))
          (recur (z/next loc) rewritten)))

      :else (recur (z/next loc) rewritten))))

(defn merge-requires
  "Make the target serve every require the moved forms use.

  For each source libspec the moved forms use: a libspec for the target
  namespace itself becomes plain names; a library the target already
  requires under another alias rewrites the moved forms' qualifiers; a
  missing library is inserted at its sorted position. Refuses an alias the
  target binds to a different library, referred symbols the target does not
  refer, and :rename or :refer :all libspecs."
  [{:keys [source-ns-form target target-ns form-sources]}]
  (let [source-specs (header/libspec-entries source-ns-form)
        target-specs (header/libspec-entries (ns-form-source target))]
    (cond
      (not (:ok source-specs)) source-specs
      (not (:ok target-specs)) (assoc target-specs :file :target)
      :else
      (let [free (set (header/form-free-symbols form-sources))
            target-by-ns (into {} (map (juxt (comp str :namespace) identity))
                               (:entries target-specs))
            target-alias->ns (into {} (keep (fn [{:keys [alias namespace]}]
                                              (when alias [(str alias) (str namespace)])))
                                   (:entries target-specs))
            used (filterv #(header/libspec-used? % form-sources) (:entries source-specs))
            decisions
            (for [{:keys [namespace alias referred renamed]} used
                  :let [lib (str namespace)
                        alias (some-> alias str)
                        used-refers (when (vector? referred)
                                      (filterv free (map str referred)))
                        existing (get target-by-ns lib)
                        existing-alias (some-> existing :alias str)]]
              (cond
                renamed {:unsupported {:namespace lib :reason :rename}}
                (= :all referred) {:unsupported {:namespace lib :reason :refer-all}}
                (= lib target-ns) {:rewrites (cond-> {lib nil} alias (assoc alias nil))}

                existing
                (let [target-refers (:referred existing)
                      missing (if (= :all target-refers)
                                []
                                (vec (remove (set (map str target-refers)) used-refers)))]
                  (cond-> {:rewrites (cond
                                       (nil? alias) {}
                                       (nil? existing-alias) {alias lib}
                                       (not= alias existing-alias) {alias existing-alias}
                                       :else {})}
                    (seq missing) (assoc :missing-refers {:namespace lib :symbols missing})))

                (and alias (contains? target-alias->ns alias))
                {:conflict {:alias alias
                            :source-namespace lib
                            :target-namespace (get target-alias->ns alias)}}

                :else
                {:add {:namespace lib :alias alias :refers used-refers}}))
            unsupported (keep :unsupported decisions)
            conflicts (keep :conflict decisions)
            missing-refers (keep :missing-refers decisions)]
        (cond
          (seq unsupported)
          {:ok false
           :error-type :unsupported-require
           :error "A moved form uses a libspec shape that cannot be merged"
           :libspecs (vec unsupported)}

          (seq conflicts)
          {:ok false
           :error-type :alias-conflict
           :error "The target binds an alias the moved forms use to a different library"
           :conflicts (vec conflicts)
           :next-step "Rename the alias in the source or the target, then move again"}

          (seq missing-refers)
          {:ok false
           :error-type :refer-merge-unsupported
           :error "The moved forms use referred symbols the target does not refer"
           :missing-refers (vec missing-refers)}

          :else
          (let [rewrites (apply merge {target-ns nil} (keep :rewrites decisions))
                results (mapv #(rewrite-qualifiers % rewrites) form-sources)
                additions (keep :add decisions)
                target' (reduce (fn [text {:keys [namespace alias refers]}]
                                  (require-ops/insert-into-require-sorted
                                    text (symbol namespace) alias refers))
                                target
                                additions)]
            {:ok true
             :target target'
             :form-sources (mapv :source results)
             :requires-added (mapv :namespace additions)
             :qualifiers-rewritten (vec (distinct (mapcat :rewritten results)))}))))))

;; ============================================================
;; What a symbol means in a namespace
;; ============================================================

(def ^:private special-forms
  #{"if" "do" "let*" "fn*" "loop*" "recur" "throw" "try" "catch" "finally"
    "quote" "var" "def" "new" "set!" "." "letfn*" "case*" "monitor-enter"
    "monitor-exit" "import*" "deftype*" "reify*" "&"})

(def ^:private core-names
  (into special-forms (map str) (keys (ns-publics 'clojure.core))))

(defn- core-filter
  "The :refer-clojure :exclude / :only options of a namespace."
  [text]
  (let [clause (some-> (ns-zloc text) (ns-clause ":refer-clojure") z/sexpr)
        options (when clause (apply hash-map (rest clause)))]
    {:exclude (set (map str (:exclude options)))
     :only (some->> (:only options) (map str) set)}))

(defn- namespace-context
  "What unqualified symbols resolve to in a namespace: its definitions, its
  referred symbols, and its view of clojure.core."
  [text project-aliases extra-definitions]
  (let [specs (header/libspec-entries (ns-form-source text))]
    {:ns-name (some-> (ns-zloc text) z/down z/right z/string)
     :definitions (into (set extra-definitions)
                        (comp (filter #(defining? project-aliases %))
                              (map :name))
                        (top-level-forms text))
     :refers (into {}
                   (for [{:keys [namespace referred]} (:entries specs)
                         :when (vector? referred)
                         symbol referred]
                     [(str symbol) (str namespace)]))
     :core (core-filter text)}))

(defn- resolve-symbol [{:keys [ns-name definitions refers core]} sym]
  (cond
    (contains? definitions sym) (str ns-name "/" sym)
    (contains? refers sym) (str (get refers sym) "/" sym)
    (and (contains? core-names sym)
         (not (contains? (:exclude core) sym))
         (or (nil? (:only core)) (contains? (:only core) sym)))
    (str "clojure.core/" sym)
    :else nil))

;; ============================================================
;; Require graph
;; ============================================================

(defn- required-namespaces
  "Namespaces named in the :require and :use clauses of an ns form, read
  leniently: vectors, bare symbols, and prefix lists. A clause that cannot be
  read as data (a reader conditional) contributes every symbol it holds, so
  the graph can only gain edges, never lose one."
  [ns-loc]
  (set
    (for [clause (keep #(ns-clause ns-loc %) [":require" ":use"])
          lib (try
                (for [entry (rest (z/sexpr clause))
                      lib (cond
                            (symbol? entry) [entry]
                            (vector? entry) [(first entry)]
                            (seq? entry) (for [child (rest entry)]
                                           (symbol (str (first entry) "."
                                                        (if (vector? child) (first child) child))))
                            :else [])]
                  lib)
                (catch Exception _
                  (->> (tree-seq n/inner? n/children (z/node clause))
                       (filter #(= :token (n/tag %)))
                       (map n/string)
                       (remove #(re-find #"^[:\"\\\d#]" %)))))]
      (str lib))))

(defn- ns-requires
  "[namespace required-namespaces] read from the ns form a file starts with,
  without parsing the rest of it; nil when there is none. A file that cannot
  be read cannot be required, so it cannot close a cycle."
  [text]
  (when-let [ns-text (move-callers/leading-ns-text text)]
    (try
      (let [loc (ns-zloc ns-text)]
        [(some-> loc z/down z/right z/string) (required-namespaces loc)])
      (catch Exception _ nil))))

(defn- find-cycle
  "A require path from `start` back to itself, or nil."
  [graph start]
  (loop [stack [[start [start]]]
         seen #{}]
    (when-let [[node path] (peek stack)]
      (let [stack (pop stack)
            nexts (get graph node #{})]
        (if (and (> (count path) 1) (= node start))
          path
          (if (and (contains? seen node) (not= node start))
            (recur stack seen)
            (recur (into stack (for [next-node (sort nexts)
                                     :when (or (= next-node start)
                                               (not (contains? seen next-node)))]
                                 [next-node (conj path next-node)]))
                   (conj seen node))))))))

;; ============================================================
;; Source side: the require back to the moved forms
;; ============================================================

(defn- require-target-from-source [text target-ns refers]
  (if (empty? refers)
    text
    (let [specs (header/libspec-entries (ns-form-source text))
          existing (some #(when (= target-ns (str (:namespace %))) %) (:entries specs))]
      (if existing
        (move-callers/add-refers text target-ns refers)
        (let [aliases (:aliases (header/source-aliases (ns-form-source text)))]
          (require-ops/insert-into-require-sorted
            text (symbol target-ns) (header/allocate-alias target-ns aliases) refers))))))

;; Requires the move leaves unused
;; ------------------------------------------------------------

(defn- drop-unused-requires
  "Remove source libspecs that only the moved forms used. A libspec with no
  alias and no refers may be loaded for its side effects, so it stays."
  [source' moved-texts]
  (let [specs (:entries (header/libspec-entries (ns-form-source source')))
        remaining [(str/replace-first source' (ns-form-source source') "")]
        unused (->> specs
                    (filter #(or (:alias %) (vector? (:referred %))))
                    (filter #(header/libspec-used? % moved-texts))
                    (remove #(header/libspec-used? % remaining))
                    (mapv (comp str :namespace)))]
    {:source (if (seq unused) (remove-libspecs source' unused) source')
     :removed unused}))

;; ============================================================
;; Checks
;; ============================================================

(defn- token-matches [text pattern]
  (loop [loc (z/of-string text) found []]
    (cond
      (z/end? loc) found
      (= :token (n/tag (z/node loc)))
      (recur (z/next loc) (if-let [m (re-matches pattern (z/string loc))]
                            (conj found m)
                            found))
      :else (recur (z/next loc) found))))

(defn- forward-uses
  "For forms in order, the names each one uses that are defined later."
  [ordered-names deps]
  (let [position (zipmap ordered-names (range))]
    (->> ordered-names
         (keep (fn [form-name]
                 (let [later (->> (get deps form-name)
                                  (filter #(when-let [p (position %)]
                                             (> p (position form-name))))
                                  (sort-by position)
                                  vec)]
                   (when (seq later) [form-name later]))))
         vec)))

(defn- refusal [error-type error & {:as data}]
  (merge {:ok false
          :error-type error-type
          :error error
          :source-unchanged true
          :target-unchanged true
          :files-unchanged true}
         data))

;; ============================================================
;; The complete move
;; ============================================================

(defn compile-move
  "Plan moving `forms` from `source` into the existing `target` at `anchor`.
  Pure: returns the complete future texts of both files, or a refusal.

  Options: :source-file :source :target-file :target :target-ns :forms
  :anchor {:above|:below FORM, :at-start|:at-end true} :project-aliases
  :workspace-sources {path text}, and optional :public-forms."
  [{:keys [source-file source target-file target target-ns forms anchor
           project-aliases workspace-sources]
    :or {project-aliases {} workspace-sources {}}
    :as opts}]
  (let [anchor-check (resolve-anchor target anchor project-aliases)
        plan (when (:ok anchor-check)
               (extract/compile-plan
                 {:file source-file
                  :source source
                  :forms forms
                  :to target-file
                  :target-ns target-ns
                  :workspace-sources workspace-sources
                  :require-policy :copy-all
                  :quoted-var-proof false
                  :public-forms (vec (:public-forms opts))
                  :derive-required-public-forms (not (contains? opts :public-forms))}))
        moved (set (map str forms))
        source-ns (some-> (ns-zloc source) z/down z/right z/string)
        source-zloc (analyze/string->zloc source)
        source-deps (into {} (map (juxt :name :depends-on))
                          (analyze/intra-ns-deps source-zloc))
        target-definitions (into #{} (comp (filter #(defining? project-aliases %))
                                           (map :name))
                                 (top-level-forms target))]
    (cond
      (not (:ok anchor-check)) (merge anchor-check {:source-unchanged true
                                                    :target-unchanged true})
      (:error plan) (assoc plan :source-unchanged true :target-unchanged true)

      (seq (:missing-required-public-forms plan))
      (refusal :required-public-forms-missing
               "Every moved private form called by remaining source must be public"
               :required-public-forms (:required-public-forms plan)
               :missing-public-forms (:missing-required-public-forms plan))

      (seq (set/intersection moved target-definitions))
      (refusal :name-clash "The target already defines a moved name"
               :names (vec (sort (set/intersection moved target-definitions))))

      :else
      (let [left-behind (->> (sort-by :line (:_form-texts plan))
                             (keep (fn [{form-name :name}]
                                     (let [calls (->> (get source-deps form-name)
                                                      (remove moved)
                                                      sort vec)]
                                       (when (seq calls) {:form form-name :calls calls}))))
                             vec)
            rewrite (fn [path text mode]
                      (move-callers/rewrite-caller
                        {:file path :text text :mode mode :moved moved
                         :source-ns source-ns :target-ns target-ns :target-file target-file}))
            ;; Every caller spells the source namespace: in a libspec, an
            ;; alias declaration or a qualified name. A prefix-list libspec
            ;; spells only its last segment.
            last-segment (last (str/split source-ns #"\."))
            candidate? (fn [text]
                         (and (some #(str/includes? text %) moved)
                              (or (str/includes? text source-ns)
                                  (str/includes? text last-segment))))
            rewrite-callers (fn []
                              (let [target-self (rewrite target-file target :target)
                                    results (->> (dissoc workspace-sources source-file target-file)
                                                 (sort-by key)
                                                 (filter (comp candidate? val))
                                                 (mapv (fn [[path text]] (rewrite path text :caller))))]
                                {:target-self target-self
                                 :unrewritable (vec (mapcat :problems (cons target-self results)))
                                 :callers (filterv :changed results)}))
            {:keys [target-self unrewritable callers]} (when (empty? left-behind)
                                                         (rewrite-callers))]
        (cond
          (seq left-behind)
          (refusal :left-behind-dependencies
                   "A moved form calls a form that stays in the source"
                   :left-behind left-behind
                   :suggested-forms (->> (:_form-texts plan)
                                         (mapcat #(map :name (:forms (analyze/extraction-closure
                                                                       source-zloc (:name %)))))
                                         (concat moved (mapcat :calls left-behind))
                                         distinct sort vec)
                   :next-step "Move the suggested forms together, or keep the moved form in the source")

          (seq unrewritable)
          (refusal :caller-not-rewritable
                   "A reference to a moved form cannot be rewritten exactly"
                   :references unrewritable
                   :next-step "Change or remove each listed reference, then move again")

          :else
          (let [ordered (vec (sort-by :line (:_form-texts plan)))
                public (set (:public-forms plan))
                texts (mapv #(if (contains? public (:name %))
                               (extract/publicize-defn-source (:text %))
                               (:text %))
                            ordered)
                merged (merge-requires {:source-ns-form (ns-form-source source)
                                        :target (:text target-self)
                                        :target-ns target-ns
                                        :form-sources texts})]
            (if-not (:ok merged)
              (assoc merged :source-unchanged true :target-unchanged true)
              (let [source-context (namespace-context source project-aliases [])
                    target-context (namespace-context (:target merged) project-aliases moved)
                    changes (->> (header/form-free-symbols texts)
                                 (remove #(str/includes? % "/"))
                                 (remove moved)
                                 sort
                                 (keep (fn [sym]
                                         (let [before (resolve-symbol source-context sym)
                                               after (resolve-symbol target-context sym)
                                               before (if (= before (str target-ns "/" sym))
                                                        after
                                                        before)]
                                           (when (not= before after)
                                             {:symbol sym
                                              :in-source (or before "unresolved")
                                              :in-target (or after "unresolved")}))))
                                 vec)]
                (if (seq changes)
                  (refusal :meaning-change
                           "A moved form would mean something different in the target"
                           :changes changes)
                  (let [names (mapv :name ordered)
                        moved-deps (into {} (for [form-name names]
                                              [form-name (set/intersection
                                                           (get source-deps form-name #{})
                                                           moved)]))
                        declared (vec (distinct (mapcat second (forward-uses names moved-deps))))
                        block (str/join "\n\n"
                                        (cond->> (:form-sources merged)
                                          (seq declared)
                                          (cons (str "(declare " (str/join " " declared) ")"))))
                        placed (place-block (:target merged) anchor project-aliases block)
                        target' (:target placed)
                        cleaned (drop-unused-requires
                                  (extract/source-without-forms source ordered)
                                  texts)
                        source' (-> (require-target-from-source
                                      (:source cleaned) target-ns (:_source-referred-forms plan))
                                    str/trimr
                                    (str "\n"))
                        graph (into {}
                                    (keep ns-requires)
                                    (concat (vals (merge (dissoc workspace-sources source-file target-file)
                                                    (into {} (map (juxt :file :text)) callers)))
                                            [source' target']))
                        cycle (or (find-cycle graph source-ns) (find-cycle graph target-ns))
                        target-order (mapv :name (filter #(defining? project-aliases %)
                                                         (top-level-forms target')))
                        target-deps (into {} (for [[form-name text] (map vector names (:form-sources merged))]
                                               [form-name (set (filter target-definitions
                                                                       (header/form-free-symbols [text])))]))
                        late-uses (forward-uses target-order target-deps)
                        auto-keywords (->> texts
                                           (mapcat #(token-matches % #"::[^/]+"))
                                           distinct sort vec)
                        warnings (cond-> []
                                   (seq auto-keywords)
                                   (conj {:type :auto-resolved-keywords-moved
                                          :keywords auto-keywords
                                          :meaning (str "These now name the target namespace instead of "
                                                        source-ns)})
                                   (seq callers)
                                   (into (mapcat :warnings callers))

                                   (seq late-uses)
                                   (into (for [[form-name uses] late-uses]
                                           {:type :used-before-definition
                                            :form form-name
                                            :uses uses})))]
                    (cond
                      (not (:ok placed)) (assoc placed :source-unchanged true :target-unchanged true)

                      cycle
                      (refusal :circular-require
                               "The move would make namespaces require each other"
                               :cycle cycle)

                      :else
                      (do
                        (parser/parse-string-all source')
                        (parser/parse-string-all target')
                        {:ok true
                         :source source'
                         :target target'
                         :moved-forms names
                         :public-forms (:public-forms plan)
                         :declared declared
                         :requires-added (:requires-added merged)
                         :requires-removed (:removed cleaned)
                         :qualifiers-rewritten (:qualifiers-rewritten merged)
                         :source-referred-forms (:_source-referred-forms plan)
                         :warnings warnings
                         :callers (into {} (map (juxt :file :text)) callers)
                         :callers-rewritten (mapv (fn [{:keys [file rewritten requires-added requires-removed]}]
                                                    {:file file
                                                     :rewritten (count rewritten)
                                                     :requires-added requires-added
                                                     :requires-removed requires-removed})
                                                  callers)
                         :callers-to-review (mapv :file callers)}))))))))))))
