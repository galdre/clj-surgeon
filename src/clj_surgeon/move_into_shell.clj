(ns clj-surgeon.move-into-shell
  "File boundary for moving forms into an existing namespace
  (docs/plans/move-into-existing-namespace.md). Reads one snapshot of every
  file, delegates every decision to the pure clj-surgeon.move-into planner,
  then writes each planned file (source, target and rewritten callers)
  atomically, lints them together, and restores them all on a new clj-kondo
  error. Receipts keep the original bytes of every file."
  (:require
   [clj-surgeon.extract :as extract]
   [clj-surgeon.file-ops :as file-ops]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.lint-gate :as lint-gate]
   [clj-surgeon.move-callers :as move-callers]
   [clj-surgeon.move-into :as move-into]
   [clj-surgeon.quoted-var-refs :as quoted-var-refs]
   [clj-surgeon.receipt-mac :as receipt-mac]
   [clj-surgeon.structural-lens :as structural-lens]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; ============================================================
;; Snapshot and plan
;; ============================================================

(def ^:private anchor-option-keys [:above :below :at-start :at-end])

(defn- canonical-path [file]
  (.getCanonicalPath (io/file file)))

(defn- workspace-root
  "The nearest directory above `file` holding a deps.edn, or its own directory."
  [file]
  (let [canonical (.getCanonicalFile (io/file file))]
    (or (some->> (iterate #(some-> % .getParentFile) canonical)
                 (take-while some?)
                 (filter #(.exists (io/file % "deps.edn")))
                 first)
        (.getParentFile canonical))))

(defn- workspace-sources
  "Every Clojure file under the workspace's declared source roots, or under
  `source-roots` when given, keyed by canonical path. Build output and tool
  directories are not callers."
  [file source-roots]
  (->> (if source-roots
         (quoted-var-refs/workspace-files (str (workspace-root file)) source-roots)
         (quoted-var-refs/workspace-files (str (workspace-root file))))
       (map (fn [f] [(.getPath f) (slurp f)]))
       (into (sorted-map))))

(defn- target-namespace [target]
  (some-> (move-into/ns-form-source target)
          (str/replace #"^\(ns\s+" "")
          (str/split #"[\s)]" 2)
          first))

(defn- relative-to [root path]
  (let [prefix (str root "/")]
    (if (str/starts-with? path prefix) (subs path (count prefix)) path)))

(defn- relativize
  "Name every reported file relative to the workspace root. Future texts and
  hashes keep their canonical keys."
  [result root]
  (let [rel (partial relative-to (str root))
        file-entry #(cond-> % (:file %) (update :file rel))]
    (cond-> result
      (:callers-rewritten result) (update :callers-rewritten (partial mapv file-entry))
      (:callers-to-review result) (update :callers-to-review (partial mapv rel))
      (:warnings result) (update :warnings (partial mapv file-entry))
      (:references result) (update :references (partial mapv file-entry))
      (:changed result) (update :changed (partial mapv rel)))))

(defn- future-files
  "Every file the plan writes: {canonical path future text}."
  [{:keys [source-file target-file source target callers]}]
  (merge callers {source-file source target-file target}))

(defn plan
  "Plan a move into an existing namespace without writing. The result holds
  every future text and the hash of every file it read them from."
  [{:keys [file to forms source-roots] :as opts}]
  (let [target-file (io/file to)
        roots-refusal (move-callers/source-roots-refusal
                        source-roots
                        #(quoted-var-refs/unusable-source-roots (str (workspace-root file)) %))]
    (cond
      roots-refusal roots-refusal

      (not (.exists target-file))
      {:ok false
       :error-type :into-existing-target-missing
       :error "--into-existing needs a target file that exists"
       :to to
       :next-step "Omit --into-existing to create a new namespace file"
       :source-unchanged true
       :target-unchanged true
       :files-unchanged true}

      (= (canonical-path file) (canonical-path to))
      {:ok false
       :error-type :extraction-path-collision
       :error "--file and --to name the same file"
       :source-unchanged true
       :target-unchanged true
       :files-unchanged true}

      :else
      (let [source-file (canonical-path file)
            target-path (canonical-path to)
            [scanned scan-error] (try [(workspace-sources file source-roots) nil]
                                      (catch Exception error [nil (.getMessage error)]))]
        (if scan-error
          {:ok false
           :error-type :workspace-scan-failed
           :error (str "The workspace's source roots could not be read: " scan-error)
           :source-unchanged true
           :target-unchanged true
           :files-unchanged true}
          (let [sources (merge scanned
                          {source-file (slurp source-file) target-path (slurp target-path)})
                result (move-into/compile-move
                         (cond-> {:source-file source-file
                                  :source (get sources source-file)
                                  :target-file target-path
                                  :target (get sources target-path)
                                  :target-ns (target-namespace (get sources target-path))
                                  :forms forms
                                  :anchor (select-keys opts anchor-option-keys)
                                  :project-aliases (forms/load-project-aliases to (str (workspace-root file)))
                                  :workspace-sources sources}
                           (contains? opts :public-forms)
                           (assoc :public-forms (:public-forms opts))))]
            (if-not (:ok result)
              (relativize result (workspace-root file))
              (let [result (-> result
                               (relativize (workspace-root file))
                               (assoc :source-file source-file :target-file target-path
                                      :workspace-root (str (workspace-root file))))]
                (assoc result :snapshot
                       (into (sorted-map)
                             (map (fn [path] [path (structural-lens/source-hash (get sources path))]))
                             (keys (future-files result))))))))))))

(defn- summary [result]
  (select-keys result [:moved-forms :public-forms :declared :requires-added :requires-removed
                       :qualifiers-rewritten :source-referred-forms :warnings
                       :callers-rewritten :callers-to-review]))

(defn plan-summary
  "The plan as the CLI prints it: no file contents. Rewrites of callers in
  other files are proposals until extract! accepts them."
  [opts]
  (let [result (plan opts)]
    (if (:ok result)
      (-> (summary result)
          (dissoc :callers-rewritten)
          (assoc :ok true :file (:file opts) :to (:to opts)
                 :callers-proposed (:callers-rewritten result)))
      (dissoc result :source :target :callers))))

;; ============================================================
;; Apply, receipt, undo
;; ============================================================

(defn- receipt-refusal
  "Refuse a receipt path that is not a new .edn file, or that names a file
  the move writes."
  [receipt-out paths]
  (when receipt-out
    (let [receipt-path (.getCanonicalPath (io/file receipt-out))
          refusal (fn [error-type error]
                    {:ok false :error-type error-type :error error :receipt-out receipt-out
                     :source-unchanged true :target-unchanged true :files-unchanged true})]
      (cond
        (contains? (set (map #(.getCanonicalPath (io/file %)) paths)) receipt-path)
        (refusal :extraction-receipt-alias "The receipt path names a file the move writes")

        (not (str/ends-with? receipt-path ".edn"))
        (refusal :invalid-extraction-receipt-path "The receipt path must end in .edn")

        (.exists (io/file receipt-path))
        (refusal :extraction-receipt-exists "The receipt file already exists")))))

(defn- write-receipt! [receipt-out entries]
  (when receipt-out
    (let [receipt {:kind :move-into
                   :files (mapv (fn [{:keys [path original result]}]
                                  {:path path
                                   :original original
                                   :result-hash (structural-lens/source-hash result)})
                                entries)}]
      (.mkdirs (.getParentFile (.getAbsoluteFile (io/file receipt-out))))
      (file-ops/atomic-write! (io/file receipt-out) (pr-str (receipt-mac/sign receipt)))
      (.getCanonicalPath (io/file receipt-out)))))

(defn- restore! [entries]
  (doseq [{:keys [path original]} entries]
    (file-ops/atomic-write! (io/file path) original))
  (every? #(= (:original %) (slurp (:path %))) entries))

(defn apply-plan!
  "Write a plan from `plan`. Every file must still match the hash it was
  planned from. A new clj-kondo error in any written file restores them all."
  [{:keys [file to receipt-out lint callers]} result]
  (let [decision (move-callers/callers-decision callers)
        ;; clj-kondo runs in the workspace, with its config and cache.
        lint (or lint #(lint-gate/clj-kondo-lint % {:dir (:workspace-root result)}))]
    (cond
      (not (:ok result))
      (dissoc result :source :target :callers)

      (map? decision) decision

      (receipt-refusal receipt-out (keys (future-files result)))
      (receipt-refusal receipt-out (keys (future-files result)))

      (and (seq (:callers result)) (not= :accept-proposed decision))
      (move-callers/decisions-required
        (:callers-rewritten result)
        "Re-run extract! with --callers accept-proposed to write these rewrites with the move, or change those callers first")

      :else
      (let [entries (mapv (fn [[path text]]
                            {:path path :original (slurp path) :result text})
                          (future-files result))
            changed (->> entries
                         (remove #(= (get-in result [:snapshot (:path %)])
                                     (structural-lens/source-hash (:original %))))
                         (mapv :path))]
        (if (seq changed)
          {:ok false
           :error-type :stale-source
           :error "A file changed after planning; nothing was written"
           :changed (mapv (partial relative-to (:workspace-root result)) changed)
           :source-unchanged true
           :target-unchanged true
           :files-unchanged true}
          (let [files (mapv (comp io/file :path) entries)
                baseline (lint files)]
            (try
              (doseq [{:keys [path result]} entries]
                (file-ops/atomic-write! (io/file path) result))
              (when-not (every? #(= (:result %) (slurp (:path %))) entries)
                (throw (ex-info "Read-back verification failed"
                                {:error-type :move-read-back-failed})))
              (let [after (if (:checked baseline) (lint files) baseline)
                    introduced (when (and (:checked baseline) (:checked after))
                                 (lint-gate/new-blocking-findings (:findings baseline) (:findings after)))]
                (cond
                  ;; The files were checked before the write; a check that cannot
                  ;; finish after it proves nothing, so the write does not stand.
                  (and (:checked baseline) (not (:checked after)))
                  {:ok false
                   :error-type :lint-unverified
                   :error (str "clj-kondo could not check the move (" (:reason after) "); every file was restored")
                   :restored (restore! entries)}

                  (seq introduced)
                  {:ok false
                   :error-type :lint-regression
                   :error "The move introduced clj-kondo errors or unresolved references; every file was restored"
                   :new-errors (vec (take 10 introduced))
                   :restored (restore! entries)}

                  :else
                  (cond-> (assoc (summary result)
                                 :ok true
                                 :applied true
                                 :file file
                                 :to to
                                 :files-written (count entries)
                                 :lint (if (:checked after)
                                         {:checked true :files (count files) :new-errors 0}
                                         {:checked false :reason (:reason after)}))
                    receipt-out (assoc :receipt-file (write-receipt! receipt-out entries)))))
              (catch Exception error
                {:ok false
                 :error-type (or (:error-type (ex-data error)) :move-write-failed)
                 :error (.getMessage error)
                 :restored (restore! entries)}))))))))

(defn execute!
  "Plan and apply a move into an existing namespace. Refusals leave every
  file unchanged; a new clj-kondo error after the write restores them all."
  [opts]
  (apply-plan! opts (plan opts)))

(defn undo!
  "Restore both files from a move-into receipt while each still matches the
  move's result. Other receipts go to the extraction undo."
  [{:keys [receipt] :as opts}]
  (let [data (try (edn/read-string (slurp receipt)) (catch Exception _ nil))]
    (cond
      (not (= :move-into (:kind data)))
      (extract/undo! opts)

      (not (receipt-mac/authentic? data))
      (receipt-mac/refusal)

      :else
      (let [changed (->> (:files data)
                         (remove #(= (:result-hash %)
                                     (structural-lens/source-hash (slurp (:path %)))))
                         (mapv :path))]
        (if (seq changed)
          {:ok false
           :error-type :receipt-result-changed
           :error "A file changed after the move; nothing was restored"
           :changed changed}
          {:ok true
           :restored (restore! (:files data))
           :files (mapv :path (:files data))})))))
