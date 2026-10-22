(ns clj-surgeon.extract
  "Extract forms from one namespace to a new namespace file.

   Algorithm:
   1. Find named forms with exact boundaries
   2. Compile a dependency-minimal target ns form
   3. Write new file with forms in topological order; forms in a dependency
      cycle follow in source order, declared first when used before definition
   4. Remove forms from source file, and their names from source declares
   5. Add a source require only when remaining forms call moved Vars
   6. Report callers that may need updating

   Does NOT: fix circular deps or update callers in other namespaces.

   ALL PLANNING IS PURE. Only execute! writes files."
  (:require
   [clj-surgeon.analyze :as analyze]
   [clj-surgeon.cljc.require-ops :as require-ops]
   [clj-surgeon.extract-header :as extract-header]
   [clj-surgeon.file-ops :as file-ops]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.lint-gate :as lint-gate]
   [clj-surgeon.move-callers :as move-callers]
   [clj-surgeon.outline :as outline]
   [clj-surgeon.quoted-var-refs :as quoted-var-refs]
   [clj-surgeon.receipt-mac :as receipt-mac]
   [clj-surgeon.structural-lens :as structural-lens]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [rewrite-clj.parser :as parser]
   [rewrite-clj.zip :as z]))

;; @spec MCP-OP-PLAN-007
(defn publicize-defn-source
  "Losslessly change one exact defn- source form to defn. Refuse all other
  declaration shapes so visibility changes cannot silently alter metadata or
  custom macro semantics."
  [source]
  (let [form (z/of-string source)
        head (z/down form)]
    (when-not (= "defn-" (z/string head))
      (throw (ex-info "Only exact defn- forms can be made public"
                      {:error-type :unsupported-public-form
                       :form-head (z/string head)})))
    (-> head
        (z/replace (parser/parse-string "defn"))
        z/root-string)))

;; ============================================================
;; Pure helpers
;; ============================================================

(defn- source-paths-from-deps-edn
  "Read :paths and alias :extra-paths from deps.edn. Returns nil if no deps.edn."
  []
  (let [f (io/file "deps.edn")]
    (when (.exists f)
      (let [deps (edn/read-string (slurp f))]
        (distinct
          (concat (:paths deps)
                  (mapcat :extra-paths (vals (:aliases deps)))))))))

(defn file-path->ns-name
  "Derive namespace name from a file path.
   src/writer/state/distillery.clj → writer.state.distillery
   /tmp/foo/src/my/app.clj → my.app
   src/clj/myapp/core.clj → myapp.core (with source-paths [\"src/clj\"])
   src/cljs/myapp/ui.cljs → myapp.ui (with source-paths [\"src/cljs\"])"
  ([path] (file-path->ns-name path nil))
  ([path source-paths]
   (let [source-paths (or (seq source-paths)
                          (source-paths-from-deps-edn)
                          ["src"])
         ;; Normalize: strip leading ./
         norm (str/replace path #"^\.\/" "")
         ;; For absolute paths, extract everything after the source root
         ;; For relative paths, match against source-paths directly
         match-root (fn [root]
                      (cond
                        ;; Relative path starting with root/
                        (str/starts-with? norm (str root "/"))
                        (subs norm (+ (count root) 1))
                        ;; Absolute path containing /root/
                        (str/includes? norm (str "/" root "/"))
                        (let [i (str/index-of norm (str "/" root "/"))]
                          (subs norm (+ i (count root) 2)))
                        :else nil))
         ;; Try all source paths, pick longest match (most specific root)
         matched (->> source-paths
                      (keep (fn [root] (when-let [rel (match-root root)]
                                         {:root root :relative rel})))
                      (sort-by #(count (:root %)) >)
                      first)
         relative (if matched
                    (:relative matched)
                    ;; Last-resort fallback: old /src/ splitting behavior
                    (let [src-idx (str/index-of norm "/src/")]
                      (if src-idx
                        (subs norm (+ src-idx 5))
                        (if (str/starts-with? norm "src/")
                          (subs norm 4)
                          norm))))]
     (-> relative
         (str/replace #"\.clj[sc]?$" "")
         (str/replace "/" ".")
         (str/replace "_" "-")))))

(defn- project-root-for-source
  [file source-paths]
  (let [path (-> file io/file .getCanonicalFile .toPath)
        roots (or (seq source-paths) ["src" "test" "dev"])]
    (or
      (some (fn [ancestor]
              (when (some #(.startsWith path (.resolve ancestor (str %))) roots)
                (.toFile ancestor)))
            (take-while some? (iterate #(.getParent %) (.getParent path))))
      (some (fn [ancestor]
              (when (.exists (io/file (.toFile ancestor) "deps.edn"))
                (.toFile ancestor)))
            (take-while some? (iterate #(.getParent %) (.getParent path))))
      (some-> file io/file .getParentFile .getParentFile))))

(defn- add-require-to-ns
  "Add one optional alias and refer entry while preserving source trivia."
  [file-source new-ns-name alias referred]
  (require-ops/insert-into-require-sorted file-source
                                          (symbol new-ns-name)
                                          (some-> alias symbol)
                                          (mapv symbol referred)))

(defn- source-line-chunks
  "Split source into line chunks while retaining every line terminator."
  [source]
  (let [chunks (vec (str/split source #"(?<=\n)" -1))]
    (if (and (seq chunks) (= "" (peek chunks)))
      (pop chunks)
      chunks)))

(defn- removal-range
  [line-chunks {:keys [comment-start end-line]}]
  (let [trailing-line end-line
        end (if (and (< trailing-line (count line-chunks))
                     (str/blank? (nth line-chunks trailing-line)))
              (inc trailing-line)
              trailing-line)]
    {:start comment-start :end end}))

(defn- remove-form-ranges
  "Remove every planned form against one immutable source snapshot.
   A following line is removed only when it is actually blank."
  [source form-texts]
  (let [line-chunks (source-line-chunks source)
        ranges (sort-by :start > (map #(removal-range line-chunks %) form-texts))]
    (->> ranges
         (reduce (fn [chunks {:keys [start end]}]
                   (when-not (<= 0 start end (count chunks))
                     (throw (ex-info "Extraction range is outside the source snapshot"
                                     {:error-type :invalid-extraction-range
                                      :start start
                                      :end end
                                      :line-count (count chunks)})))
                   (into (subvec chunks 0 start)
                         (subvec chunks end)))
                 line-chunks)
         (apply str))))

(defn- declare-locations
  "Every top-level `(declare ...)` in source, with its symbol names and
  1-based start and end rows."
  [source]
  (loop [zloc (z/of-string source {:track-position? true})
         found []]
    (if (nil? zloc)
      found
      (recur (z/right zloc)
             (if (and (z/list? zloc)
                      (= "declare" (some-> zloc z/down z/string)))
               (let [children (->> (z/down zloc) z/right
                                   (iterate z/right)
                                   (take-while some?))
                     [[start-row] [end-row]] (z/position-span zloc)]
                 (conj found {:start-row start-row
                              :end-row end-row
                              :symbols (->> children
                                            (filter #(symbol? (z/sexpr %)))
                                            (mapv z/string))
                              :only-symbols? (every? #(symbol? (z/sexpr %))
                                                     children)}))
               found)))))

(defn- drop-moved-declares
  "Remove moved names from the source's top-level `declare` forms. A declare
  left naming nothing is removed with its line, like a moved form."
  [source moved-names]
  (let [moved? (set moved-names)
        ;; First remove moved symbols from declares that keep another name.
        partial-edited
        (loop [zloc (z/of-string source)]
          (let [zloc (if (and (z/list? zloc)
                              (= "declare" (some-> zloc z/down z/string)))
                       (loop [form zloc
                              child (z/right (z/down zloc))]
                         (cond
                           (nil? child) form
                           (and (moved? (z/string child))
                                (some #(not (moved? %))
                                      (->> (z/down form) z/right
                                           (iterate z/right)
                                           (take-while some?)
                                           (map z/string))))
                           (let [form' (z/up (z/remove child))]
                             (recur form' (z/right (z/down form'))))
                           :else (recur form (z/right child))))
                       zloc)]
            (if-let [next-zloc (z/right zloc)]
              (recur next-zloc)
              (z/root-string zloc))))
        ;; Then remove declares whose every name moved.
        emptied (->> (declare-locations partial-edited)
                     (filter #(and (:only-symbols? %)
                                   (seq (:symbols %))
                                   (every? moved? (:symbols %))))
                     (map (fn [{:keys [start-row end-row]}]
                            {:comment-start (dec start-row)
                             :end-line end-row})))]
    (if (seq emptied)
      (remove-form-ranges partial-edited emptied)
      partial-edited)))

(defn source-without-forms
  "The source with the planned forms removed, and their names removed from
  its declare forms. Pure."
  [source form-ranges]
  (-> (remove-form-ranges source form-ranges)
      (drop-moved-declares (map :name form-ranges))))

(defn- validate-complete-source!
  [file source]
  (try
    (parser/parse-string-all source)
    source
    (catch Exception e
      (throw (ex-info "Extraction candidate is not complete Clojure source"
                      {:error-type :invalid-extraction-candidate
                       :file file
                       :source-unchanged true}
                      e)))))

(defn compile-candidates
  "Purely compile and parse the complete source and target after extraction."
  [{:keys [source source-file target-file form-ranges target-source target-ns
           target-alias source-referred-forms]}]
  (let [source-without-forms (source-without-forms source form-ranges)
        future-source (if (seq source-referred-forms)
                        (add-require-to-ns source-without-forms
                                           target-ns
                                           target-alias
                                           source-referred-forms)
                        source-without-forms)]
    {:source (validate-complete-source! source-file future-source)
     :target (validate-complete-source! target-file target-source)}))

(def ^:private receipt-version
  "2 adds :callers, the files a caller rewrite changed. Undo accepts 1."
  2)

(defn- canonical-path
  [file]
  (.getCanonicalPath (io/file file)))

(defn- receipt-refusal
  [receipt-out source-file target-file]
  (when receipt-out
    (let [receipt-path (canonical-path receipt-out)
          source-path (canonical-path source-file)
          target-path (canonical-path target-file)]
      (cond
        (not (str/ends-with? receipt-path ".edn"))
        {:error "Extraction receipt path must end in .edn"
         :error-type :invalid-extraction-receipt-path
         :source-unchanged true
         :target-unchanged true}

        (#{source-path target-path} receipt-path)
        {:error "Extraction receipt must not alias a source or target file"
         :error-type :extraction-receipt-alias
         :source-unchanged true
         :target-unchanged true}

        (.exists (io/file receipt-path))
        {:error "Extraction receipt already exists"
         :error-type :extraction-receipt-exists
         :source-unchanged true
         :target-unchanged true}))))

(defn- extraction-receipt
  [{:keys [source-file target-file original-source future-source target-source callers]}]
  {:receipt-version receipt-version
   :operation :extract!
   :source {:file (canonical-path source-file)
            :source-hash (structural-lens/source-hash original-source)
            :result-hash (structural-lens/source-hash future-source)
            :original-source original-source
            :result-source future-source}
   :target {:file (canonical-path target-file)
            :absent-before true
            :result-hash (structural-lens/source-hash target-source)
            :result-source target-source}
   :callers (mapv (fn [{:keys [path original result]}]
                    {:file (canonical-path path)
                     :original-source original
                     :result-hash (structural-lens/source-hash result)
                     :result-source result})
                  callers)
   :inverse {:operation :undo-extract!}})

(defn- publish-receipt!
  [receipt-out receipt]
  (when receipt-out
    (let [receipt-file (io/file receipt-out)]
      (.mkdirs (.getParentFile (.getAbsoluteFile receipt-file)))
      (file-ops/atomic-write! receipt-file (pr-str receipt))
      (when-not (= receipt (edn/read-string (slurp receipt-file)))
        (throw (ex-info "Extraction receipt read-back verification failed"
                        {:error-type :extraction-receipt-read-back-failed})))
      (canonical-path receipt-file))))

;; ============================================================
;; Pure: Build extraction plan
;; ============================================================

;; @spec MCP-OP-PLAN-006
(defn compile-plan
  "Purely compile an extraction plan from one source snapshot and a captured
  workspace source map. No file, process, clock, or registry access occurs.
  :quoted-var-proof false skips the quoted-Var scan for a caller that
  rewrites those references itself."
  [{:keys [file source forms to target-ns workspace-sources require-policy
           public-forms derive-required-public-forms quoted-var-proof]
    :or {workspace-sources {} require-policy :minimal public-forms []
         derive-required-public-forms false quoted-var-proof true}}]
  (let [lines (vec (str/split-lines source))
        ol (outline/outline-source file source)
        all-forms (:forms ol)
        source-ns (some-> (:ns ol) str)
        form-names (set (map str forms))
        matched (->> all-forms
                     (filter #(and (contains? form-names (str (:name %)))
                                   (not= 'declare (:type %))))
                     vec)
        missing (set/difference
                  form-names
                  (set (map #(str (:name %)) matched)))]
    (cond
      (seq missing)
      {:error (str "Forms not found: " (str/join ", " (sort missing)))}

      (nil? source-ns)
      {:error "Could not determine source namespace"}

      (nil? target-ns)
      {:error "Could not determine target namespace"}

      :else
      (let [src-zloc (z/of-string source {:track-position? true})
            ns-zloc (loop [location src-zloc]
                      (when location
                        (if (and (z/list? location)
                                 (= "ns" (some-> location z/down z/string)))
                          location
                          (recur (z/right location)))))
            ns-form-text (when ns-zloc (z/string ns-zloc))
            zloc (analyze/string->zloc source)
            extracted-names (set (map #(str (:name %)) matched))
            topo-order (let [topology (analyze/topological-sort zloc)]
                         (->> (:sorted topology)
                              (filter extracted-names)))
            form-texts
            (->> (sort-by :line matched)
                 (mapv
                   (fn [form]
                     (let [form-start
                           (let [index (dec (dec (:line form)))]
                             (loop [line-index index]
                               (if (neg? line-index)
                                 0
                                 (if (str/starts-with?
                                       (str/trim (nth lines line-index "")) ";")
                                   (recur (dec line-index))
                                   (inc line-index)))))
                           form-end (:end-line form)]
                       {:name (str (:name form))
                        :type (str (:type form))
                        :line (:line form)
                        :end-line form-end
                        :comment-start form-start
                        :text (str/join "\n"
                                        (subvec lines form-start form-end))}))))
            texts-by-name (into {} (map (juxt :name identity) form-texts))
            ;; Forms in a dependency cycle, and forms that depend on one, never
            ;; reach the topological order. They follow it in source order, and
            ;; any of them used before its definition is declared first.
            ordered-names (into (vec topo-order)
                                (->> form-texts
                                     (map :name)
                                     (remove (set topo-order))))
            ordered-texts (mapv #(get texts-by-name %) ordered-names)
            forward-declared
            (let [deps (into {}
                             (map (juxt :name :depends-on))
                             (analyze/intra-ns-deps zloc))
                  position (zipmap ordered-names (range))]
              (->> ordered-names
                   (mapcat (fn [form-name]
                             (->> (get deps form-name)
                                  (filter #(when-let [p (position %)]
                                             (> p (position form-name))))
                                  (sort-by position))))
                   distinct
                   vec))
            header-result
            (extract-header/compile-target-header
              {:source-ns-form ns-form-text
               :target-ns target-ns
               :form-sources (mapv :text ordered-texts)
               :require-policy require-policy})
            alias-result (if (= :copy-all require-policy)
                           {:ok true :aliases {}}
                           (extract-header/source-aliases ns-form-text))
            target-alias (when (and (:ok alias-result)
                                    (not= :copy-all require-policy))
                           (extract-header/allocate-alias
                             target-ns (:aliases alias-result)))
            remaining-callers
            (extract-header/remaining-source-callers source extracted-names)
            ;; A protocol's methods and a record's constructors move with
            ;; their form; the source refers those its remaining forms call.
            {defined-vars :vars defined-classes :classes}
            (move-callers/defined-vars-by-form source)
            moved-extra (set (mapcat #(get defined-vars %) extracted-names))
            source-referred
            (->> (extract-header/form-free-symbols
                   [(source-without-forms source form-texts)])
                 (filter moved-extra)
                 (concat (extract-header/source-referred-forms remaining-callers))
                 distinct sort vec)
            private-form-names
            (->> matched
                 (filter #(forms/private-form? (str (:type %))))
                 (map #(str (:name %)))
                 set)
            supported-public-form-names
            (->> matched
                 (filter #(= "defn-" (str (:type %))))
                 (map #(str (:name %)))
                 set)
            required-public-forms
            (set/intersection private-form-names (set source-referred))
            requested-public-forms
            (if derive-required-public-forms
              required-public-forms
              (set (map str public-forms)))
            invalid-public-forms
            (set/difference requested-public-forms private-form-names)
            unsupported-public-forms
            (set/difference requested-public-forms
                            supported-public-form-names)
            missing-required-public-forms
            (set/difference required-public-forms requested-public-forms)
            publicized-texts
            (if (seq unsupported-public-forms)
              ordered-texts
              (mapv (fn [form]
                      (if (contains? requested-public-forms (:name form))
                        (update form :text publicize-defn-source)
                        form))
                    ordered-texts))
            new-file-content
            (when (:ok header-result)
              (str
                (str/join "\n\n"
                          (concat [(:ns-form header-result)]
                                  (when (seq forward-declared)
                                    [(str "(declare "
                                          (str/join " " forward-declared)
                                          ")")])
                                  (map :text publicized-texts)))
                "\n"))
            captured-sources (assoc workspace-sources (str file) source)
            other-files
            (->> captured-sources
                 (remove #(= (str file) (str (key %))))
                 (filter (fn [[_ content]]
                           (some #(str/includes? content (str %))
                                 extracted-names)))
                 (map (comp str key))
                 sort
                 vec)
            subjects (mapv #(str source-ns "/" %) (sort extracted-names))
            quoted-proof (if quoted-var-proof
                           (quoted-var-refs/scan-sources captured-sources subjects)
                           {:ok true :locations []})]
        (cond
          (seq invalid-public-forms)
          {:error "public-forms must name selected private forms"
           :error-type :invalid-public-forms
           :invalid-public-forms (vec (sort invalid-public-forms))}

          (seq unsupported-public-forms)
          {:error "One or more selected private forms cannot be publicized losslessly"
           :error-type :unsupported-public-forms
           :unsupported-public-forms (vec (sort unsupported-public-forms))}

          (not (:ok header-result))
          header-result

          (not (:ok alias-result))
          alias-result

          (not (:ok quoted-proof))
          (assoc quoted-proof
                 :error "Quoted Var caller proof failed; extraction was not planned")

          :else
          {:file file
           :to to
           :source-ns source-ns
           :target-ns target-ns
           :target-alias target-alias
           :require-policy (:require-policy header-result)
           :copied-require-count (:copied-require-count header-result)
           :target-requires (:target-requires header-result)
           :omitted-target-requires (:omitted-target-requires header-result)
           :remaining-source-callers remaining-callers
           :source-referred-forms source-referred
           :required-public-forms (vec (sort required-public-forms))
           :missing-required-public-forms
           (vec (sort missing-required-public-forms))
           :public-forms (vec (sort requested-public-forms))
           :source-require-added (boolean (seq source-referred))
           :forms-to-extract (mapv :name form-texts)
           :form-count (count matched)
           :lines-extracted
           (reduce + (map #(- (:end-line %) (dec (:comment-start %)))
                          form-texts))
           :new-file-preview
           (let [preview-lines (str/split-lines new-file-content)]
             (if (> (count preview-lines) 20)
               (str (str/join "\n" (take 20 preview-lines)) "\n... ("
                    (count preview-lines) " lines total)")
               new-file-content))
           :callers-to-review other-files
           :quoted-var-references
           (mapv #(select-keys % [:subject :file :line :character
                                  :reference-authority])
                 (:locations quoted-proof))
           :_source source
           :_source-hash (structural-lens/source-hash source)
           :_new-file-content new-file-content
           :_form-texts form-texts
           :moved-vars (vec (sort (into extracted-names moved-extra)))
           :moved-classes (vec (sort (filter defined-classes extracted-names)))
           :_source-referred-forms source-referred})))))

(defn with-caller-rewrites
  "Rewrite every caller of the extracted Vars in `sources` {path text} to
  the new namespace, deciding every refusal before any write. Pure: returns
  the plan with :_callers {path future-text}, :callers-proposed and
  :callers-to-review, or a refusal. The rewrites are proposals: execute!
  writes them only when the request accepts them. Options as for
  move-into/compile-move: :caller-authority :model returns unrewritable
  references as :caller-problems evidence instead of refusing and keeps the
  proposal out of the cycle check; :max-candidates bounds the scan."
  ([plan sources] (with-caller-rewrites plan sources {}))
  ([{:keys [file to source-ns target-ns moved-vars moved-classes] :as plan} sources
    {:keys [caller-authority max-candidates] :or {caller-authority :kernel}}]
   (if (:error plan)
     plan
     (let [kernel? (= :kernel caller-authority)
           {:keys [callers problems budget-exceeded]}
           (move-callers/rewrite-callers
             {:sources sources :exclude [(str file) (str to)]
              :source-ns source-ns :target-ns target-ns :target-file (str to)
              :moved moved-vars :moved-classes moved-classes
              :max-candidates max-candidates})
           caller-texts (into {} (map (juxt :file :text)) callers)
           future-source (when (and (not budget-exceeded) (or (not kernel?) (empty? problems)))
                           (:source (compile-candidates
                                      {:source (:_source plan)
                                       :source-file file
                                       :target-file to
                                       :form-ranges (:_form-texts plan)
                                       :target-source (:_new-file-content plan)
                                       :target-ns target-ns
                                       :target-alias (:target-alias plan)
                                       :source-referred-forms (:_source-referred-forms plan)})))
           graph (when future-source
                   (into {}
                         (keep move-callers/ns-requires)
                         (concat (vals (merge (dissoc sources (str file) (str to))
                                              (when kernel? caller-texts)))
                                 [future-source (:_new-file-content plan)])))
           cycle (some-> graph (move-callers/find-cycle target-ns))
           refusal {:source-unchanged true :target-unchanged true :files-unchanged true}]
       (cond
         budget-exceeded
         (merge refusal (move-callers/budget-refusal budget-exceeded))

         (and kernel? (seq problems))
         (merge refusal
                {:error "A reference to an extracted form cannot be rewritten exactly"
                 :error-type :caller-not-rewritable
                 :references problems
                 :next-step "Change or remove each listed reference, then extract again"})

         cycle
         (merge refusal
                {:error "Rewriting the callers would make namespaces require each other"
                 :error-type :circular-require
                 :cycle cycle})

         :else
         (assoc plan
                :_callers caller-texts
                :callers-proposed (move-callers/caller-summary callers)
                :callers-to-review (mapv :file callers)
                :caller-problems (if kernel? [] (vec problems))
                :caller-warnings (vec (mapcat :warnings callers))))))))

(defn- relative-to [root path]
  (let [prefix (str root "/")]
    (if (str/starts-with? (str path) prefix) (subs (str path) (count prefix)) (str path))))

(defn- relativize-callers
  "Name reported caller files relative to the project root."
  [plan root]
  (let [rel (partial relative-to root)
        file-entry #(cond-> % (:file %) (update :file rel))]
    (cond-> plan
      (:callers-proposed plan) (update :callers-proposed (partial mapv file-entry))
      (:callers-to-review plan) (update :callers-to-review (partial mapv rel))
      (:caller-warnings plan) (update :caller-warnings (partial mapv file-entry))
      (:references plan) (update :references (partial mapv file-entry)))))

(defn plan
  "Capture one workspace snapshot and delegate extraction decisions to
  compile-plan and with-caller-rewrites. This is the filesystem shell, not
  the pure planner. Callers are found under the project's declared source
  roots, or under :source-roots (relative to the project) when given.
  Without :public-forms, every moved private form that remaining source still
  calls is made public, as on the MCP route. An explicit :public-forms list is
  used as given, and execute! refuses one that omits a required form."
  [{:keys [file forms to source-paths require-policy public-forms source-roots]
    :or {require-policy :minimal}
    :as opts}]
  (if-let [roots-refusal (move-callers/source-roots-refusal
                           source-roots
                           #(quoted-var-refs/unusable-source-roots
                              (.getCanonicalPath (project-root-for-source file source-paths)) %))]
    roots-refusal
    (try
      (let [source (slurp file)
            target-ns (file-path->ns-name to source-paths)
            project-root (.getCanonicalPath (project-root-for-source file source-paths))
            source-canonical-path (.getCanonicalPath (io/file file))
            target-canonical-path (.getCanonicalPath (io/file to))
            workspace-sources
            (->> (if source-roots
                   (quoted-var-refs/workspace-files project-root source-roots)
                   (quoted-var-refs/workspace-files project-root))
                 (remove #(= source-canonical-path (.getPath %)))
                 (map (fn [workspace-file]
                        [(.getPath workspace-file) (slurp workspace-file)]))
                 (into (sorted-map)))
            compiled (compile-plan
                       {:file source-canonical-path
                        :source source
                        :forms forms
                        :to target-canonical-path
                        :target-ns target-ns
                        :workspace-sources workspace-sources
                        :require-policy require-policy
                        :public-forms (vec public-forms)
                        :derive-required-public-forms (not (contains? opts :public-forms))})
            planned (with-caller-rewrites compiled workspace-sources)]
        (cond-> (relativize-callers planned project-root)
          (not (:error planned))
          (assoc :file file
                 :to to
                 :_caller-hashes (into {} (map (fn [[path _]]
                                                 [path (structural-lens/source-hash
                                                         (get workspace-sources path))]))
                                       (:_callers planned))
                 :_project-root project-root)))
      (catch Exception error
        {:ok false
         :error-type :extraction-snapshot-failed
         :error (.getMessage error)
         :source-unchanged true
         :target-unchanged true}))))

;; ============================================================
;; Effects: Execute the extraction
;; ============================================================

(defn execute!
  "Execute an extraction plan.
   Every future file is compiled and parsed before any file is written. The
   source and every rewritten caller are hash-fenced. A failed commit, or a
   new clj-kondo error or unresolved reference in any written file, restores
   the source and the callers and removes the newly-created target."
  [{:keys [file to receipt-out lint callers] :as opts}]
  (let [p (plan opts)
        decision (move-callers/callers-decision callers)
        ;; clj-kondo runs in the project, with its config and cache.
        lint (or lint #(lint-gate/clj-kondo-lint % {:dir (:_project-root p)}))]
    (cond
      (:error p)
      p

      (map? decision)
      decision

      (and (seq (:_callers p)) (not= :accept-proposed decision))
      (move-callers/decisions-required
        (:callers-proposed p)
        "Re-run extract! with --callers accept-proposed to write these rewrites with the extraction, or change those callers first")

      (seq (:missing-required-public-forms p))
      {:error "Every moved private form called by remaining source must be public"
       :error-type :required-public-forms-missing
       :required-public-forms (:required-public-forms p)
       :missing-public-forms (:missing-required-public-forms p)
       :next-step "Add the missing forms to :public-forms, or omit :public-forms to publicize every required form"
       :source-unchanged true
       :target-unchanged true}

      :else
      (let [original-source (:_source p)
            original-source-hash (:_source-hash p)
            new-content (:_new-file-content p)
            form-texts (:_form-texts p)
            source-referred-forms (:_source-referred-forms p)
            target-alias (:target-alias p)
            target-ns (:target-ns p)
            candidates (compile-candidates
                         {:source original-source
                          :source-file file
                          :target-file to
                          :form-ranges form-texts
                          :target-source new-content
                          :target-ns target-ns
                          :target-alias target-alias
                          :source-referred-forms source-referred-forms})
            updated-source (:source candidates)
            target-file (io/file to)
            source-file (io/file file)
            callers (mapv (fn [[path text]] {:path path :original (slurp path) :result text})
                          (:_callers p))
            changed-callers (->> callers
                                 (remove #(= (get-in p [:_caller-hashes (:path %)])
                                             (structural-lens/source-hash (:original %))))
                                 (mapv #(relative-to (:_project-root p) (:path %))))
            receipt-error (receipt-refusal receipt-out source-file target-file)
            receipt (receipt-mac/sign
                      (extraction-receipt
                        {:source-file source-file
                         :target-file target-file
                         :original-source original-source
                         :future-source updated-source
                         :target-source new-content
                         :callers callers}))
            existing-files (into [source-file] (map (comp io/file :path)) callers)
            restore! (fn []
                       (let [source-restored?
                             (try
                               (file-ops/atomic-write! source-file original-source)
                               (= original-source (slurp source-file))
                               (catch Exception _ false))
                             callers-restored?
                             (every? (fn [{:keys [path original]}]
                                       (try
                                         (file-ops/atomic-write! (io/file path) original)
                                         (= original (slurp path))
                                         (catch Exception _ false)))
                                     callers)
                             target-removed?
                             (or (not (.exists target-file))
                                 (and (= new-content (slurp target-file))
                                      (.delete target-file)))]
                         {:source-restored source-restored?
                          :callers-restored callers-restored?
                          :target-removed target-removed?}))]
        (cond
          receipt-error
          receipt-error

          (.exists target-file)
          {:error "Extraction target already exists"
           :error-type :extraction-target-exists
           :file to
           :source-unchanged true
           :target-unchanged true}

          (not= original-source-hash
                (structural-lens/source-hash (slurp source-file)))
          {:error "Extraction source changed after planning"
           :error-type :stale-extraction-source
           :file file
           :source-unchanged true
           :target-unchanged true}

          (seq changed-callers)
          {:error "A caller changed after planning; nothing was written"
           :error-type :stale-extraction-source
           :changed changed-callers
           :source-unchanged true
           :target-unchanged true
           :files-unchanged true}

          :else
          (let [baseline (lint existing-files)]
            (.mkdirs (.getParentFile (.getAbsoluteFile target-file)))
            (try
              (file-ops/atomic-write! target-file new-content)
              (file-ops/atomic-write! source-file updated-source)
              (doseq [{:keys [path result]} callers]
                (file-ops/atomic-write! (io/file path) result))
              (when-not (and (= new-content (slurp target-file))
                             (= updated-source (slurp source-file))
                             (every? #(= (:result %) (slurp (:path %))) callers))
                (throw (ex-info "Extraction read-back verification failed"
                                {:error-type :extraction-read-back-failed})))
              (let [after (if (:checked baseline)
                            (lint (conj existing-files target-file))
                            baseline)
                    introduced (when (and (:checked baseline) (:checked after))
                                 (lint-gate/new-blocking-findings (:findings baseline)
                                                                  (:findings after)))]
                (cond
                  ;; Checked before the write but not after: the write does not stand.
                  (and (:checked baseline) (not (:checked after)))
                  {:error (str "clj-kondo could not check the extraction (" (:reason after)
                               "); every file was restored")
                   :error-type :lint-unverified
                   :restored (every? true? (vals (restore!)))}

                  (seq introduced)
                  {:error "The extraction introduced clj-kondo errors or unresolved references; every file was restored"
                   :error-type :lint-regression
                   :new-errors (vec (take 10 introduced))
                   :restored (every? true? (vals (restore!)))}

                  :else
                  (let [receipt-file (publish-receipt! receipt-out receipt)]
                    (cond->
                      {:file file
                       :to to
                       :target-requires (:target-requires p)
                       :omitted-target-requires (:omitted-target-requires p)
                       :remaining-source-callers (:remaining-source-callers p)
                       :source-referred-forms source-referred-forms
                       :public-forms (:public-forms p)
                       :log (vec (concat
                                   [{:action :create-file
                                     :file to
                                     :forms (count form-texts)
                                     :lines (count (str/split-lines new-content))}]
                                   (map (fn [form]
                                          {:action :remove-form
                                           :form (:name form)
                                           :from-line (:line form)})
                                        (sort-by :line > form-texts))
                                   (when (seq source-referred-forms)
                                     [{:action :add-require
                                       :ns target-ns
                                       :alias target-alias
                                       :refer source-referred-forms}])))
                       :verified {:source-hash
                                  (structural-lens/source-hash original-source)
                                  :source-result-hash
                                  (structural-lens/source-hash updated-source)
                                  :target-result-hash
                                  (structural-lens/source-hash new-content)
                                  :parsed true
                                  :atomic-write true
                                  :read-back true}
                       :callers-rewritten (:callers-proposed p)
                       :callers-to-review (:callers-to-review p)
                       :warnings (:caller-warnings p)
                       :files-written (+ 2 (count callers))
                       :lint (if (:checked after)
                               {:checked true :files (inc (count existing-files)) :new-errors 0}
                               {:checked false :reason (:reason after)})
                       :quoted-var-references (:quoted-var-references p)
                       :summary {:forms-extracted (count form-texts)
                                 :new-file-lines (count (str/split-lines new-content))
                                 :source-require-added
                                 (boolean (seq source-referred-forms))
                                 :callers-to-review (count (:callers-to-review p))
                                 :quoted-var-references
                                 (count (:quoted-var-references p))}}
                      receipt-file (assoc :receipt-file receipt-file)))))
              (catch Exception commit-error
                (let [{:keys [source-restored target-removed callers-restored]} (restore!)
                      source-restored? source-restored
                      target-removed? target-removed
                      receipt-file (some-> receipt-out io/file)
                      receipt-removed?
                      (or (nil? receipt-file)
                          (not (.exists receipt-file))
                          (and (= (pr-str receipt) (slurp receipt-file))
                               (.delete receipt-file)))]
                  (throw (ex-info "Extraction commit failed and was rolled back"
                                  {:error-type :extraction-commit-failed
                                   :source-restored source-restored?
                                   :callers-restored callers-restored
                                   :target-removed target-removed?
                                   :receipt-removed receipt-removed?
                                   :source-unchanged source-restored?}
                                  commit-error)))))))))))

(defn undo!
  "Undo one successful extraction while both result files still match its receipt."
  [{:keys [receipt]}]
  (try
    (let [receipt-data (edn/read-string (slurp receipt))
          source (:source receipt-data)
          target (:target receipt-data)
          source-file (io/file (:file source))
          target-file (io/file (:file target))]
      (cond
        ;; An unsigned or altered receipt could overwrite any file, whichever
        ;; route wrote it.
        (not (receipt-mac/authentic? receipt-data))
        (receipt-mac/refusal)

        (= :compiled-extraction (:operation receipt-data))
        ((requiring-resolve 'clj-surgeon.mcp-extraction/undo!) receipt-data)

        (not (#{1 receipt-version} (:receipt-version receipt-data)))
        {:error "Unsupported extraction receipt version"
         :error-type :invalid-extraction-receipt
         :source-unchanged true
         :target-unchanged true}

        (not= :extract! (:operation receipt-data))
        {:error "Receipt is not an extraction receipt"
         :error-type :invalid-extraction-receipt
         :source-unchanged true
         :target-unchanged true}

        (or (not (.exists source-file)) (not (.exists target-file)))
        {:error "Extraction result files are missing"
         :error-type :stale-extraction-result
         :source-unchanged true
         :target-unchanged true}

        (not= (:result-hash source)
              (structural-lens/source-hash (slurp source-file)))
        {:error "Extraction source no longer matches the receipt"
         :error-type :stale-extraction-result
         :file (:file source)
         :source-unchanged true
         :target-unchanged true}

        (not= (:result-hash target)
              (structural-lens/source-hash (slurp target-file)))
        {:error "Extraction target no longer matches the receipt"
         :error-type :stale-extraction-result
         :file (:file target)
         :source-unchanged true
         :target-unchanged true}

        (some #(or (not (.exists (io/file (:file %))))
                   (not= (:result-hash %) (structural-lens/source-hash (slurp (:file %)))))
              (:callers receipt-data))
        {:error "A rewritten caller no longer matches the receipt"
         :error-type :stale-extraction-result
         :files (->> (:callers receipt-data)
                     (remove #(and (.exists (io/file (:file %)))
                                   (= (:result-hash %) (structural-lens/source-hash (slurp (:file %))))))
                     (mapv :file))
         :source-unchanged true
         :target-unchanged true
         :files-unchanged true}

        :else
        (let [original-source (validate-complete-source!
                                (:file source)
                                (:original-source source))
              result-source (:result-source source)
              result-target (:result-source target)]
          (if-not (.delete target-file)
            {:error "Could not remove the extraction target"
             :error-type :extraction-undo-delete-failed
             :source-unchanged true
             :target-unchanged true}
            (try
              (file-ops/atomic-write! source-file original-source)
              (doseq [{:keys [file original-source]} (:callers receipt-data)]
                (file-ops/atomic-write! (io/file file) original-source))
              (when-not (and (= (:source-hash source)
                                (structural-lens/source-hash (slurp source-file)))
                             (every? #(= (:original-source %) (slurp (:file %)))
                                     (:callers receipt-data)))
                (throw (ex-info "Extraction undo read-back verification failed"
                                {:error-type :extraction-undo-read-back-failed})))
              {:ok true
               :operation :undo-extract!
               :receipt (canonical-path receipt)
               :verified {:source-restored true
                          :callers-restored (count (:callers receipt-data))
                          :source-hash (:source-hash source)
                          :target-absent (not (.exists target-file))
                          :read-back true}}
              (catch Exception undo-error
                (let [source-restored-to-result?
                      (try
                        (file-ops/atomic-write! source-file result-source)
                        (= (:result-hash source)
                           (structural-lens/source-hash (slurp source-file)))
                        (catch Exception _ false))
                      target-restored?
                      (try
                        (file-ops/atomic-write! target-file result-target)
                        (= (:result-hash target)
                           (structural-lens/source-hash (slurp target-file)))
                        (catch Exception _ false))
                      callers-restored?
                      (every? (fn [{:keys [file result-source result-hash]}]
                                (try
                                  (file-ops/atomic-write! (io/file file) result-source)
                                  (= result-hash (structural-lens/source-hash (slurp file)))
                                  (catch Exception _ false)))
                              (:callers receipt-data))]
                  (throw (ex-info "Extraction undo failed and its result was restored"
                                  {:error-type :extraction-undo-failed
                                   :source-result-restored source-restored-to-result?
                                   :target-result-restored target-restored?
                                   :callers-result-restored callers-restored?}
                                  undo-error)))))))))
    (catch java.io.FileNotFoundException _
      {:error "Extraction receipt does not exist"
       :error-type :extraction-receipt-not-found
       :source-unchanged true
       :target-unchanged true})
    (catch RuntimeException error
      (if (:error-type (ex-data error))
        (throw error)
        {:error "Extraction receipt is invalid"
         :error-type :invalid-extraction-receipt
         :source-unchanged true
         :target-unchanged true}))))
