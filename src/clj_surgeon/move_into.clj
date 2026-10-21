(ns clj-surgeon.move-into
  "Move top-level forms into an existing namespace file.

   Plan: docs/plans/move-into-existing-namespace.md. Everything here is pure:
   source texts in, complete future texts or a typed refusal out. The CLI
   shell in clj-surgeon.extract reads the files, writes the result
   atomically, lints it, and rolls back on a new error."
  (:require
   [clj-surgeon.cljc.require-ops :as require-ops]
   [clj-surgeon.extract-header :as header]
   [clj-surgeon.forms :as forms]
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
