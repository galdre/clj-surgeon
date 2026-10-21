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
   [clj-surgeon.cljc.require-ops :as require-ops]
   [clj-surgeon.extract-header :as header]
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
  whether they sit inside a reader conditional or a runtime require call."
  [context node]
  (let [tag (n/tag node)
        head (list-head node)]
    (cond-> context
      (= :quote tag) (assoc :quote :quote)
      (= :syntax-quote tag) (assoc :quote :syntax-quote)
      (= "quote" head) (assoc :quote :quote)
      (contains? #{"require" "use" "alias" "load"} head) (assoc :runtime-require true)
      (and (= :reader-macro tag)
           (some-> (first (n/children node)) n/string (str/starts-with? "?")))
      (assoc :reader-conditional true))))

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
      (not (contains? moved text)) nil
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

(defn- scan
  "Plan the replacements, in document order, and find the problems."
  [forms ns-node {:keys [mode target-ns target-alias] :as context}]
  (reduce-tokens
    forms ns-node
    (fn [found node {:keys [quote reader-conditional runtime-require]}]
      (let [text (symbol-text node)
            {:keys [kind local]} (some-> text (classify context))
            {:keys [row col end-col]} (meta node)
            problem #(update found :problems conj {:line row :reason % :text text})
            found (cond-> (if (#{:qualified :aliased} kind)
                            found
                            (record-use found (n/string node)))
                    (and text (contains? (:moved context) (or (second (split-qualified text)) text)))
                    (update :mentions conj row))]
        (cond
          (nil? kind) found
          (= :source-ns kind) (if runtime-require (problem :runtime-require) found)
          reader-conditional (problem :reader-conditional)
          (= :refer-all kind) (if (= :target mode) found (problem :refer-all))
          (= :referred kind) (update found :referred conj {:line row :text local})
          :else
          (let [to (cond
                     (and (= :target mode) (= :quote quote)) (str target-ns "/" local)
                     (= :target mode) local
                     (= :qualified kind) (str target-ns "/" local)
                     :else (str target-alias "/" local))]
            (cond-> (update found :replacements conj
                            {:line row :col col :end-col end-col :from text :to to :kind kind})
              (and (= :quote quote) (= :aliased kind) (not= :target mode))
              (update :warnings conj {:type :quoted-alias-symbol :line row
                                      :from text :to to}))))))
    {:replacements [] :problems [] :referred [] :warnings [] :prefixes #{} :plain #{}
     :mentions []}))

(defn- line-chunks
  "Split text into lines that keep their terminators."
  [text]
  (let [chunks (vec (str/split text #"(?<=\n)" -1))]
    (if (and (seq chunks) (= "" (peek chunks))) (pop chunks) chunks)))

(defn- apply-replacements
  "Splice each planned token by its line and columns, checking that the
  original token is exactly what the scan saw."
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
         (not-any? plain (vals renamed)))))

(defn- rewrite-parsed
  [{:keys [file text source-ns target-ns target-file moved mode]} forms]
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
                 :target-alias target-alias :moved moved :source source}
        {:keys [replacements problems referred warnings mentions] :as uses}
        (scan forms ns-node context)
        renamed-moved (filter moved (keys (:renamed source)))
        moved-refers (when (vector? (:referred source)) (filterv moved (:referred source)))
        needs-target? (or (seq replacements) (seq referred))
        ns-line (some-> ns-node meta :row)
        problems (cond-> problems
                   (seq renamed-moved)
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
                              (concat replacements referred))))
        problems (mapv #(assoc % :file file) problems)]
    (if (seq problems)
      {:ok false :problems problems}
      (let [body (subs (apply-replacements text replacements)
                       (+ (count prefix) (count ns-text)))
            text' ns-text
            need-alias? (some #(= :aliased (:kind %)) replacements)
            used-refers (vec (sort (distinct (map :text referred))))
            [text' added]
            (cond
              (or (= :target mode) (not needs-target?)) [text' []]

              (nil? target)
              [(require-ops/insert-into-require-sorted
                 text' (symbol target-ns) (when need-alias? target-alias) (not-empty used-refers))
               [target-ns]]

              :else
              (let [text' (if (= :bare (:shape target)) (bare-to-vector text' target-ns) text')
                    text' (if (and need-alias? (nil? (:alias target)))
                            (add-alias text' target-ns target-alias)
                            text')]
                [(add-refers text' target-ns used-refers) []]))
            remove-source? (and (= :vector (:shape source))
                                (or (seq replacements) (seq moved-refers))
                                (unused? source-ns source moved uses))
            text' (cond
                    remove-source? (remove-libspecs text' [source-ns])
                    (seq moved-refers) (remove-refers text' source-ns moved-refers)
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
