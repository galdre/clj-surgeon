(ns clj-surgeon.move-callers
  "Rewrite one file's references to forms moved from a source namespace into
  a target namespace (docs/plans/move-into-existing-namespace.md, \"Callers\").

  Pure: one file's text in, its complete future text or the references that
  cannot be rewritten out. Aliased references take the file's alias for the
  target, fully qualified ones keep their spelling, referred names move to
  the target libspec, and a source libspec the rewrite leaves unused is
  removed. In :target mode the file is the target itself, so references
  become plain names and no require is added."
  (:require
   [clj-surgeon.analyze :as analyze]
   [clj-surgeon.cljc.require-ops :as require-ops]
   [clj-surgeon.extract-header :as header]
   [clj-surgeon.forms :as forms]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as parser]
   [rewrite-clj.zip :as z]))

;; ============================================================
;; The ns form
;; ============================================================

(defn ns-zloc
  "The top-level ns form of `text`, or nil."
  [text]
  (some->> (z/of-string text)
           (iterate z/right)
           (take-while some?)
           (filter #(and (z/list? %) (= "ns" (some-> % z/down z/string))))
           first))

(defn ns-clause
  "The clause of an ns form headed by `keyword-text`, such as \":require\"."
  [ns-loc keyword-text]
  (some->> (some-> ns-loc z/down)
           (iterate z/right)
           (take-while some?)
           (filter #(and (z/list? %) (= keyword-text (some-> % z/down z/string))))
           first))

(defn leading-ns-text
  "The text of the ns form that `text` starts with (after whitespace and
  comments), found by matching delimiters without parsing the rest of the
  file; nil when the first form is not an ns form or never closes."
  [^String text]
  (let [length (count text)
        skip-line (fn [i] (let [j (.indexOf text "\n" (int i))] (if (neg? j) length j)))
        start (loop [i 0]
                (cond
                  (>= i length) nil
                  (Character/isWhitespace (.charAt text i)) (recur (inc i))
                  (= \, (.charAt text i)) (recur (inc i))
                  (= \; (.charAt text i)) (recur (skip-line i))
                  (= \( (.charAt text i)) i
                  :else nil))]
    (when (and start (re-find #"^\(ns[\s,]" (subs text start (min length (+ start 4)))))
      (loop [i start
             depth 0]
        (when (< i length)
          (let [c (.charAt text i)]
            (case c
              \" (recur (loop [j (inc i)]
                          (cond
                            (>= j length) length
                            (= \\ (.charAt text j)) (recur (+ j 2))
                            (= \" (.charAt text j)) (inc j)
                            :else (recur (inc j))))
                        depth)
              \\ (recur (+ i 2) depth)
              \; (recur (skip-line i) depth)
              (\( \[ \{) (recur (inc i) (inc depth))
              (\) \] \}) (if (= 1 depth)
                           (subs text start (inc i))
                           (recur (inc i) (dec depth)))
              (recur (inc i) depth))))))))

(defn- clause-children [clause]
  (->> (some-> clause z/down z/right)
       (iterate z/right)
       (take-while some?)))

(defn- tokens-in [loc]
  (->> (tree-seq n/inner? n/children (z/node loc))
       (filter #(= :token (n/tag %)))
       (map n/string)))

(defn- libspec-vector-for [require-clause lib]
  (->> (clause-children require-clause)
       (filter #(and (z/vector? %) (= lib (some-> % z/down z/string))))
       first))

(defn- libspec-options
  "{:alias :referred :renamed} of a libspec vector's sexpr."
  [form]
  (let [options (apply hash-map (rest form))]
    {:alias (some-> (or (:as options) (:as-alias options)) str)
     :referred (let [r (:refer options)]
                 (if (= :all r) :all (some->> r (mapv str))))
     :renamed (some->> (:rename options) (into {} (map (fn [[k v]] [(str k) (str v)]))))}))

(defn- prefix-list-libs
  "The libraries a prefix list such as (billing invoice [customer :as c]) names."
  [loc]
  (let [[prefix & children] (z/sexpr loc)]
    (for [child children]
      (str prefix "." (if (vector? child) (first child) child)))))

(defn- names-lib? [loc lib]
  (let [node (z/node loc)]
    (case (n/tag node)
      :token (= lib (n/string node))
      :vector (= lib (some-> loc z/down z/string))
      :list (some #{lib} (prefix-list-libs loc))
      (boolean (some #{lib} (tokens-in loc))))))

(defn- read-libspecs
  "How a file's ns form requires `lib`: {:shape :vector|:bare|:use|:prefix-list
  |:reader-conditional ...options}, or nil when it does not."
  [ns-loc lib]
  (let [require-clause (ns-clause ns-loc ":require")
        entry (some #(when (names-lib? % lib) %) (clause-children require-clause))]
    (cond
      (some #(names-lib? % lib) (clause-children (ns-clause ns-loc ":use")))
      {:shape :use :referred :all}

      (nil? entry) nil
      (z/vector? entry) (assoc (libspec-options (z/sexpr entry)) :shape :vector)
      (= :token (n/tag (z/node entry))) {:shape :bare}
      (z/list? entry) {:shape :prefix-list}
      :else {:shape :reader-conditional})))

(defn- class-imports
  "Entries of the ns form's :import clause that import one of `classes` from
  `source-ns`'s package: [{:line (within the ns form) :class}]."
  [ns-loc source-ns classes]
  (let [package (str/replace source-ns "-" "_")]
    (for [entry (clause-children (ns-clause ns-loc ":import"))
          :let [node (z/node entry)
                row (:row (meta node))
                tokens (keep #(when (= :token (n/tag %)) (n/string %))
                             (tree-seq n/inner? n/children node))]
          class (sort classes)
          :when (if (#{:vector :list} (n/tag node))
                  (and (= package (first tokens)) (some #{class} (rest tokens)))
                  (= (str package "." class) (n/string node)))]
      {:line row :class class})))

(defn- file-aliases
  "alias -> namespace for every libspec vector of a file."
  [ns-loc]
  (into {}
        (for [clause [(ns-clause ns-loc ":require")]
              v (clause-children clause)
              :when (z/vector? v)
              :let [form (z/sexpr v)
                    {:keys [alias]} (libspec-options form)]
              :when alias]
          [alias (str (first form))])))

;; ============================================================
;; Libspec edits
;; ============================================================

(defn- symbols-vector-node [symbols]
  (n/vector-node (vec (interpose (n/spaces 1) (map (comp n/token-node symbol) symbols)))))

(defn- edit-libspec
  "Apply f to the zipper at `lib`'s libspec vector and return the root text."
  [text lib f]
  (-> (libspec-vector-for (ns-clause (ns-zloc text) ":require") lib)
      f
      z/root-string))

(defn- option-loc [libspec-loc option]
  (->> (z/down libspec-loc)
       (iterate z/right)
       (take-while some?)
       (filter #(= option (z/string %)))
       first))

(defn- append-option [libspec-loc option value-node]
  (-> libspec-loc
      (z/append-child* (n/spaces 1))
      (z/append-child* (n/keyword-node option))
      (z/append-child* (n/spaces 1))
      (z/append-child* value-node)))

(defn add-refers
  "Add `refers` to `lib`'s libspec vector in `text`."
  [text lib refers]
  (if (empty? refers)
    text
    (edit-libspec
      text lib
      (fn [libspec]
        (if-let [vector-loc (some-> (option-loc libspec ":refer") z/right)]
          (z/replace vector-loc (symbols-vector-node
                                  (distinct (concat (map str (z/sexpr vector-loc)) refers))))
          (append-option libspec :refer (symbols-vector-node refers)))))))

(defn- map-node-of
  "A map node {k v ...} of string keys and values, in the given order."
  [entries]
  (n/map-node (vec (interpose (n/spaces 1)
                              (mapcat (fn [[k v]] [(n/token-node (symbol k)) (n/token-node (symbol v))])
                                      entries)))))

(defn- add-renames
  "Add `renames` {original new-name} to `lib`'s :rename map in `text`."
  [text lib renames]
  (if (empty? renames)
    text
    (edit-libspec
      text lib
      (fn [libspec]
        (if-let [map-loc (some-> (option-loc libspec ":rename") z/right)]
          (z/replace map-loc (map-node-of (concat (map (fn [[k v]] [(str k) (str v)]) (z/sexpr map-loc))
                                                  (sort renames))))
          (append-option libspec :rename (map-node-of (sort renames))))))))

(defn- remove-renames
  "Remove `names` from `lib`'s :rename map, and the :rename option when it
  empties."
  [text lib names]
  (let [doomed (set names)]
    (edit-libspec
      text lib
      (fn [libspec]
        (let [map-loc (some-> (option-loc libspec ":rename") z/right)
              kept (remove (comp doomed str key) (z/sexpr map-loc))]
          (if (seq kept)
            (z/replace map-loc (map-node-of (map (fn [[k v]] [(str k) (str v)]) kept)))
            ;; z/remove moves to the previous value, the :rename keyword.
            (-> map-loc z/remove z/remove)))))))

(defn- add-alias [text lib alias]
  (edit-libspec text lib #(append-option % :as (n/token-node (symbol alias)))))

(defn- bare-to-vector
  "Replace a bare `lib` symbol in :require with the vector [lib]."
  [text lib]
  (-> (->> (clause-children (ns-clause (ns-zloc text) ":require"))
           (filter #(and (= :token (n/tag (z/node %))) (= lib (z/string %))))
           first)
      (z/replace (n/vector-node [(n/token-node (symbol lib))]))
      z/root-string))

(defn- remove-refers
  "Remove `names` from `lib`'s :refer vector, and the :refer option when it
  empties."
  [text lib names]
  (let [doomed (set names)]
    (edit-libspec
      text lib
      (fn [libspec]
        (let [refer-loc (option-loc libspec ":refer")
              vector-loc (some-> refer-loc z/right)
              kept (remove doomed (map str (z/sexpr vector-loc)))]
          (if (seq kept)
            (z/replace vector-loc (symbols-vector-node kept))
            ;; z/remove moves to the previous value, the :refer keyword.
            (-> vector-loc z/remove z/remove)))))))

(defn remove-libspecs
  "Remove the :require entries naming `namespaces`, and the :require clause
  itself when nothing is left in it."
  [text namespaces]
  (let [doomed (set namespaces)
        clause-loc (ns-clause (ns-zloc text) ":require")]
    (loop [loc (some-> clause-loc z/down z/right)
           clause clause-loc]
      (cond
        (nil? loc)
        (if (some-> clause z/down z/right)
          (z/root-string clause)
          (-> clause z/remove z/root-string))

        (or (and (z/vector? loc) (contains? doomed (some-> loc z/down z/string)))
            (and (= :token (n/tag (z/node loc))) (contains? doomed (z/string loc))))
        ;; z/remove lands on the previous node depth-first, which may sit
        ;; inside the previous libspec; climb back to the :require clause.
        (let [clause' (loop [l (z/remove loc)]
                        (if (and (z/list? l) (= ":require" (some-> l z/down z/string)))
                          l
                          (recur (z/up l))))]
          (recur (some-> clause' z/down z/right) clause'))

        :else (recur (z/right loc) clause)))))

;; ============================================================
;; References
;; ============================================================

(def ^:private gap-tags #{:whitespace :newline :comma :comment :uneval})

(defn- list-head [node]
  (when (= :list (n/tag node))
    (some #(when-not (gap-tags (n/tag %))
             (when (= :token (n/tag %)) (n/string %)))
          (take 3 (n/children node)))))

(defn- child-context
  "The context a node gives its children: the nearest enclosing quote, and
  whether they sit inside a comment form or a runtime require call."
  [context node]
  (let [tag (n/tag node)
        head (list-head node)]
    (cond-> context
      (= :quote tag) (assoc :quote :quote)
      (= :syntax-quote tag) (assoc :quote :syntax-quote)
      (= "quote" head) (assoc :quote :quote)
      (= "comment" head) (assoc :comment true)
      (contains? #{"require" "use" "alias" "load"} head) (assoc :runtime-require true))))

(defn- reduce-tokens
  "Reduce f over every token node of the top-level `forms` other than
  `ns-node`, in document order: (f acc token-node context)."
  [forms ns-node f init]
  (letfn [(visit [acc node context]
            (if (= :token (n/tag node))
              (f acc node context)
              (if (n/inner? node)
                (let [context (child-context context node)]
                  (reduce #(visit %1 %2 context) acc (n/children node)))
                acc)))]
    (reduce #(if (identical? ns-node %2) %1 (visit %1 %2 {})) init forms)))

(defn- symbol-text [node]
  (let [text (n/string node)]
    (when-not (re-find #"^[:\"\\\d#]" text) text)))

(defn- split-qualified [text]
  (when-let [[_ prefix local] (re-matches #"([^/]+)/(.+)" text)]
    [prefix local]))

(defn- platform [file]
  (some->> file (re-find #"\.(clj[sc]?)$") second keyword))

(defn- target-serves? [target-file caller-file]
  (let [target (platform target-file)]
    (or (= :cljc target) (= target (platform caller-file)))))

(defn- classify
  "What one symbol token means for the move: nil when unrelated."
  [text {:keys [source-ns moved source]}]
  (let [[prefix local] (split-qualified text)]
    (cond
      (and prefix (contains? moved local) (= prefix source-ns)) {:kind :qualified :local local}
      (and prefix (contains? moved local) (some? (:alias source)) (= prefix (:alias source)))
      {:kind :aliased :local local}
      prefix nil
      (= text source-ns) {:kind :source-ns}
      (some #(and (contains? moved (key %)) (= text (val %))) (:renamed source))
      {:kind :renamed :local (key (some #(when (= text (val %)) %) (:renamed source))) :as text}
      (not (contains? moved text)) nil
      (contains? (:renamed source) text) nil
      (and (vector? (:referred source)) (some #{text} (:referred source))) {:kind :referred :local text}
      (= :all (:referred source)) {:kind :refer-all :local text}
      :else nil)))

(defn- record-use
  "Note what a token that stays in the file uses: the qualifier of a symbol
  or auto-resolved keyword, or a plain symbol."
  [found text]
  (if-let [[_ prefix] (re-matches #"(?:::)?([^:/\"\\][^/]*)/.+" text)]
    (update found :prefixes conj prefix)
    (if (re-find #"^[:\"\\\d#]" text)
      found
      (update found :plain conj text))))

(defn- replace-token
  "Plan the replacement of one qualified or aliased reference."
  [found node local quote {:keys [mode target-ns target-alias source-ns]}]
  (let [text (n/string node)
        {:keys [row col end-col]} (meta node)
        kind (if (str/starts-with? text (str source-ns "/")) :qualified :aliased)
        to (cond
             (and (= :target mode) (= :quote quote)) (str target-ns "/" local)
             (= :target mode) local
             (= :qualified kind) (str target-ns "/" local)
             :else (str target-alias "/" local))]
    (cond-> (update found :replacements conj
                    {:line row :col col :end-col end-col :from text :to to :kind kind})
      (and (= :quote quote) (= :aliased kind) (not= :target mode))
      (update :warnings conj {:type :quoted-alias-symbol :line row :from text :to to}))))

(defn- scan
  "Plan the replacements, in document order, and find the problems."
  [forms ns-node {:keys [mode] :as context}]
  (reduce-tokens
    forms ns-node
    (fn [found node {:keys [quote comment runtime-require]}]
      (let [text (symbol-text node)
            {:keys [kind local] :as classified} (some-> text (classify context))
            row (:row (meta node))
            problem #(update found :problems conj {:line row :reason % :text text})
            found (cond-> (if (#{:qualified :aliased} kind)
                            found
                            (record-use found (n/string node)))
                    (and text (contains? (:class-names context) text))
                    (update :class-references conj {:line row :text text})
                    (and text (contains? (:moved context) (or (second (split-qualified text)) text)))
                    (update :mentions conj row))]
        (cond
          (nil? kind) found
          (= :source-ns kind) (cond
                                (not runtime-require) found
                                ;; A comment form is never evaluated.
                                comment (update found :warnings conj
                                                {:type :runtime-require-in-comment :line row})
                                :else (problem :runtime-require))
          (= :target mode) (if (#{:qualified :aliased} kind)
                             (replace-token found node local quote context)
                             found)
          (#{:referred :refer-all} kind) (update found :referred conj {:line row :text local})
          (= :renamed kind) (update found :renamed conj {:line row :text text
                                                         :local local :as (:as classified)})
          :else (replace-token found node local quote context))))
    {:replacements [] :problems [] :referred [] :renamed [] :warnings [] :prefixes #{} :plain #{}
     :mentions [] :class-references []}))

(defn- line-chunks
  "Split text into lines that keep their terminators."
  [text]
  (let [chunks (vec (str/split text #"(?<=\n)" -1))]
    (if (and (seq chunks) (= "" (peek chunks))) (pop chunks) chunks)))

(defn- apply-sorted-replacements
  [text replacements]
  (if (empty? replacements)
    text
    (->> replacements
         (sort-by (juxt :line :col) #(compare %2 %1))
         (reduce (fn [lines {:keys [line col end-col from to]}]
                   (let [chunk (nth lines (dec line))
                         start (dec col)
                         end (dec end-col)]
                     (when-not (= from (subs chunk start end))
                       (throw (ex-info "A planned token is not where the scan found it"
                                       {:line line :expected from})))
                     (assoc lines (dec line) (str (subs chunk 0 start) to (subs chunk end)))))
                 (line-chunks text))
         (apply str))))

(defn locals-at-probes
  "For each probe name in `probes`, the locals in scope where it occurs in
  `text`'s top-level forms: a set, or :unknown when the form holding it cannot
  be read as data (a reader conditional, say). Forms with no probe are not
  read."
  [text probes]
  (let [wanted (set probes)]
    (reduce
      (fn [result node]
        (let [present (filterv #(str/includes? (n/string node) %) wanted)]
          (if (empty? present)
            result
            (merge result
                   (try
                     (let [found (analyze/locals-at
                                   (n/sexpr node {:auto-resolve (fn [alias] (symbol (str alias)))})
                                   present)
                           known (:known-locals found)]
                       ;; A probe the analysis never reached sits in a form it
                       ;; does not understand.
                       (into {} (map (fn [p] [p (if-let [v (found p)]
                                                  (with-meta v {:known-locals known})
                                                  :unknown)]))
                             present))
                     (catch Exception _
                       (zipmap present (repeat :unknown))))))))
      (zipmap probes (repeat nil))
      (remove #(gap-tags (n/tag %)) (n/children (parser/parse-string-all text))))))

(defn- unknown-binder?
  "Does `name` occur in the top-level form holding `probe` although the
  lexical analysis never saw it bound there? A form the analysis does not
  model, such as a record field, may bind it, and the analysis reads such a
  binding as a free reference. Conservative: a plain use of the name in the
  same form also counts."
  [text probe name known-locals]
  (and (not (contains? known-locals name))
       (some (fn [node]
               (when (str/includes? (n/string node) probe)
                 (some #(and (= :token (n/tag %)) (= name (n/string %)))
                       (tree-seq n/inner? n/children node))))
             (n/children (parser/parse-string-all text)))))

(defn capture-check
  "Plan plain names for some positions: `probe-text` holds a probe symbol at
  each, and `probe->name` names the plain name each would become. Returns the
  probes whose name is, or may be, a local there."
  [probe-text probe->name]
  (let [locals (locals-at-probes probe-text (keys probe->name))]
    (set (filter (fn [probe]
                   (let [name (probe->name probe)
                         in-scope (locals probe)]
                     (or (= :unknown in-scope)
                         (contains? in-scope name)
                         (unknown-binder? probe-text probe name
                                          (:known-locals (meta in-scope))))))
                 (keys probe->name)))))

(defn probe-name
  "A symbol name no source contains, for the i-th position under test."
  [i]
  (str "__clj_surgeon_capture_probe_" i "__"))

(defn- apply-replacements
  "Splice each planned token by its line and columns, checking that the
  original token is exactly what the scan saw. A token planned twice is
  replaced once; overlapping ranges refuse rather than splice twice."
  [text replacements]
  (let [replacements (vals (into {} (map (juxt (juxt :line :col :end-col :from :to) identity))
                                 replacements))]
    (doseq [[a b] (->> replacements (sort-by (juxt :line :col)) (partition 2 1))
            :when (and (= (:line a) (:line b)) (< (:col b) (:end-col a)))]
      (throw (ex-info "Planned replacements overlap" {:line (:line a) :tokens [(:from a) (:from b)]})))
    (apply-sorted-replacements text replacements)))

;; ============================================================
;; One file
;; ============================================================

(defn- unused?
  "Was the source libspec there for an alias or refers that no token left
  in the file uses?"
  [source-ns {:keys [alias referred renamed]} moved {:keys [prefixes plain]}]
  (let [kept-refers (when (vector? referred) (remove moved referred))]
    (and (or alias (vector? referred))
         (not (contains? prefixes source-ns))
         (not (and alias (contains? prefixes alias)))
         (not-any? plain kept-refers)
         (not-any? plain (vals (remove (comp moved key) renamed))))))

(defn- rewrite-parsed
  [{:keys [file text source-ns target-ns target-file moved moved-classes mode]} forms]
  ;; Parse once. Every ns-form read and edit works on the ns form's own text,
  ;; which is then spliced back between the text before it and the body.
  (let [moved (set (map str moved))
        ns-node (first (filter #(= "ns" (list-head %)) forms))
        ns-text (some-> ns-node n/string)
        prefix (if ns-node
                 (apply str (map n/string (take-while #(not (identical? ns-node %)) forms)))
                 "")
        ns-loc (some-> ns-text ns-zloc)
        source (some-> ns-loc (read-libspecs source-ns))
        target (some-> ns-loc (read-libspecs target-ns))
        target-alias (when (= :caller mode)
                       (or (:alias target)
                           (header/allocate-alias target-ns (or (some-> ns-loc file-aliases) {}))))
        context {:mode mode :source-ns source-ns :target-ns target-ns
                 :target-alias target-alias :moved moved :source source
                 :class-names (set (map #(str (str/replace source-ns "-" "_") "." %) moved-classes))}
        {:keys [replacements problems referred renamed warnings mentions] :as uses}
        (scan forms ns-node context)
        renamed-moved (filter moved (keys (:renamed source)))
        moved-refers (when (vector? (:referred source)) (filterv moved (:referred source)))
        needs-target? (or (seq replacements) (seq referred) (seq renamed))
        ns-line (some-> ns-node meta :row)
        problems (cond-> problems
                   (and (= :target mode) (seq renamed-moved))
                   (conj {:line ns-line :reason :rename :text (str/join " " renamed-moved)})

                   ;; The alias is unreadable, so any spelling of a moved name may
                   ;; be a reference.
                   (and (#{:prefix-list :reader-conditional} (:shape source))
                        (seq mentions))
                   (conj {:line ns-line :reason (:shape source) :text source-ns})

                   (and (= :caller mode) needs-target? (nil? ns-node))
                   (into (map (fn [{:keys [line from text]}]
                                {:line line :reason :no-ns-form :text (or from text)})
                              replacements))

                   (and (= :caller mode) needs-target? ns-node
                        (not (target-serves? target-file file)))
                   (into (map (fn [{:keys [line from text]}]
                                {:line line :reason :target-platform :text (or from text)})
                              (concat replacements referred renamed))))
        ;; In the target, a reference becomes a plain name; a local of that
        ;; name in scope would capture it.
        plain (vec (keep-indexed (fn [i r] (when (and (= :target mode) (= (:to r) (second (split-qualified (:from r)))))
                                             (assoc r :probe (probe-name i))))
                                 replacements))
        captured (when (seq plain)
                   (capture-check (apply-replacements text (map #(assoc % :to (:probe %)) plain))
                                  (into {} (map (juxt :probe :to)) plain)))
        problems (into problems
                       (for [r plain :when (contains? captured (:probe r))]
                         {:line (:line r) :reason :local-capture :text (:from r)}))
        ;; A moved record or type is a class in the source's package; an
        ;; import of it, or a class-qualified use, would dangle.
        problems (into problems
                       (concat
                         (for [{:keys [line class]} (some-> ns-loc (class-imports source-ns moved-classes))]
                           {:line (+ ns-line (dec line)) :reason :class-import :text class})
                         (for [{:keys [line text]} (:class-references uses)]
                           {:line line :reason :class-reference :text text})))
        problems (mapv #(assoc % :file file) problems)]
    (if (seq problems)
      {:ok false :problems problems}
      (let [body (subs (apply-replacements text replacements)
                       (+ (count prefix) (count ns-text)))
            text' ns-text
            need-alias? (some #(= :aliased (:kind %)) replacements)
            used-refers (vec (sort (distinct (concat (map :text referred) (map :local renamed)))))
            used-renames (into {} (map (juxt :local :as)) renamed)
            [text' added]
            (cond
              (or (= :target mode) (not needs-target?)) [text' []]

              (nil? target)
              [(-> (require-ops/insert-into-require-sorted
                     text' (symbol target-ns) (when need-alias? target-alias) (not-empty used-refers))
                   (add-renames target-ns used-renames))
               [target-ns]]

              :else
              (let [text' (if (= :bare (:shape target)) (bare-to-vector text' target-ns) text')
                    text' (if (and need-alias? (nil? (:alias target)))
                            (add-alias text' target-ns target-alias)
                            text')]
                [(-> text'
                     (add-refers target-ns used-refers)
                     (add-renames target-ns used-renames))
                 []]))
            remove-source? (and (= :vector (:shape source))
                                (or (seq replacements) (seq moved-refers) (seq renamed-moved))
                                (unused? source-ns source moved uses))
            text' (cond
                    remove-source? (remove-libspecs text' [source-ns])
                    (or (seq moved-refers) (seq renamed-moved))
                    (cond-> text'
                      (seq renamed-moved) (remove-renames source-ns renamed-moved)
                      (seq moved-refers) (remove-refers source-ns moved-refers))
                    :else text')
            text' (str prefix text' body)]
        {:ok true
         :file file
         :text text'
         :changed (not= text text')
         :rewritten (mapv #(select-keys % [:line :from :to]) replacements)
         :requires-added added
         :requires-removed (if remove-source? [source-ns] [])
         :warnings (mapv #(assoc % :file file) warnings)}))))

(defn rewrite-caller
  "Plan one file's rewrite. Options: :file :text :source-ns :target-ns
  :target-file :moved (a set of names) and :mode (:caller or :target).

  Returns {:ok true :text :changed :rewritten :requires-added
  :requires-removed :warnings}, or {:ok false :problems [{:file :line :reason
  :text}]}."
  [{:keys [file text] :as opts}]
  (let [forms (try (n/children (parser/parse-string-all text))
                   (catch Exception error
                     {:error (.getMessage error)}))]
    (if (map? forms)
      {:ok false :problems [{:file file :line nil :reason :unparsable :text (:error forms)}]}
      (rewrite-parsed opts forms))))

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

(defn ns-requires
  "[namespace required-namespaces] read from the ns form a file starts with,
  without parsing the rest of it; nil when there is none. A file that cannot
  be read cannot be required, so it cannot close a cycle."
  [text]
  (when-let [ns-text (leading-ns-text text)]
    (try
      (let [loc (ns-zloc ns-text)]
        [(some-> loc z/down z/right z/string) (required-namespaces loc)])
      (catch Exception _ nil))))

(defn find-cycle
  "A require path from `start` back to itself in `graph` {namespace
  required-namespaces}, or nil."
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
;; Every caller in a workspace
;; ============================================================

(defn defined-vars-by-form
  "{form-name [vars]} for each top-level form of `text` that defines Vars
  besides its name (protocol methods, record and type constructors), and
  the names of the forms that define a class: {:vars {...} :classes #{...}}."
  [text]
  (reduce
    (fn [result node]
      (let [head (list-head node)
            kind (forms/classify head)]
        (if-not (#{:defprotocol :defrecord :deftype} kind)
          result
          (let [form (try (n/sexpr node {:auto-resolve (fn [a] (symbol (str a)))})
                          (catch Exception _ nil))
                form-name (some-> form second str)]
            (cond-> (assoc-in result [:vars form-name] (forms/defined-vars form))
              (forms/defines-class? head) (update :classes conj form-name))))))
    {:vars {} :classes #{}}
    (n/children (parser/parse-string-all text))))

(defn rewrite-callers
  "Rewrite every file in `sources` {path text}, other than `exclude`, that
  may reference a moved Var. Every caller spells the source namespace, in a
  libspec, an alias declaration or a qualified name; a prefix-list libspec
  spells only its last segment. Returns {:callers [changed results]
  :problems [...]}."
  [{:keys [sources exclude source-ns target-ns target-file moved moved-classes]}]
  (let [last-segment (last (str/split source-ns #"\."))
        names (into (set moved) moved-classes)
        candidate? (fn [text]
                     (and (some #(str/includes? text %) names)
                          (or (str/includes? text source-ns)
                              (str/includes? text last-segment))))
        results (->> (apply dissoc sources exclude)
                     (sort-by key)
                     (filter (comp candidate? val))
                     (mapv (fn [[path text]]
                             (rewrite-caller {:file path :text text :mode :caller
                                              :moved moved :moved-classes moved-classes
                                              :source-ns source-ns :target-ns target-ns
                                              :target-file target-file}))))]
    {:callers (filterv :changed results)
     :problems (vec (mapcat :problems results))}))

(defn caller-summary
  "What a result reports about the rewritten callers."
  [callers]
  (mapv (fn [{:keys [file rewritten requires-added requires-removed]}]
          {:file file
           :rewritten (count rewritten)
           :requires-added requires-added
           :requires-removed requires-removed})
        callers))
