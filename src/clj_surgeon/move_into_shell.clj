(ns clj-surgeon.move-into-shell
  "File boundary for moving forms into an existing namespace
  (docs/plans/move-into-existing-namespace.md). Reads one snapshot of every
  file, delegates every decision to the pure clj-surgeon.move-into planner,
  then writes each planned file (source, target and rewritten callers)
  atomically, lints them together, and restores them all on a new clj-kondo
  error. Receipts keep the original bytes of every file."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clj-surgeon.diagnostic-delta :as diagnostic-delta]
   [clj-surgeon.extract :as extract]
   [clj-surgeon.file-ops :as file-ops]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.move-into :as move-into]
   [clj-surgeon.quoted-var-refs :as quoted-var-refs]
   [clj-surgeon.structural-lens :as structural-lens]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; ============================================================
;; Lint
;; ============================================================

(defn clj-kondo-lint
  "Lint `files` together with clj-kondo from PATH. Returns {:checked false}
  when clj-kondo is not installed."
  [files]
  (if-not (fs/which "clj-kondo")
    {:checked false :reason "clj-kondo is not installed"}
    (let [result (apply process/shell {:out :string :err :string :continue true}
                        "clj-kondo" "--config" "{:output {:format :edn}}"
                        "--lint" (map str files))
          parsed (try (edn/read-string (:out result)) (catch Exception _ nil))]
      (if (map? parsed)
        {:checked true :findings (vec (:findings parsed))}
        {:checked false :reason "clj-kondo output could not be read"}))))

(defn- new-errors
  "Error findings in `after` that `before` does not have, ignoring position."
  [before after]
  (let [errors #(filter (fn [finding] (= :error (keyword (:level finding)))) %)
        baseline (frequencies (map diagnostic-delta/finding-identity (errors before)))]
    (:selected
      (reduce (fn [{:keys [remaining] :as state} finding]
                (let [identity (diagnostic-delta/finding-identity finding)]
                  (if (pos? (get remaining identity 0))
                    (update-in state [:remaining identity] dec)
                    (update state :selected conj
                            (select-keys finding [:filename :row :type :message])))))
              {:remaining baseline :selected []}
              (errors after)))))

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

(defn- invalid-source-roots
  "The --source-roots entries that are not relative paths inside the workspace."
  [roots]
  (vec (remove #(and (string? %)
                     (not (.isAbsolute (io/file %)))
                     (not-any? #{".."} (str/split % #"[/\\]")))
               roots)))

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
        bad-roots (some-> source-roots invalid-source-roots not-empty)]
    (cond
      bad-roots
      {:ok false
       :error-type :invalid-source-roots
       :error "--source-roots takes paths relative to the workspace, inside it"
       :source-roots bad-roots
       :source-unchanged true
       :target-unchanged true
       :files-unchanged true}

      (not (.exists target-file))
      {:ok false
       :error-type :into-existing-target-missing
       :error "--into-existing needs a target file that exists"
       :to to
       :next-step "Omit --into-existing to create a new namespace file"
       :source-unchanged true
       :target-unchanged true
       :files-unchanged true}

      :else
      (let [source-file (canonical-path file)
            target-path (canonical-path to)
            sources (merge (workspace-sources file source-roots)
                           {source-file (slurp source-file) target-path (slurp target-path)})
            result (move-into/compile-move
                     (cond-> {:source-file source-file
                              :source (get sources source-file)
                              :target-file target-path
                              :target (get sources target-path)
                              :target-ns (target-namespace (get sources target-path))
                              :forms forms
                              :anchor (select-keys opts anchor-option-keys)
                              :project-aliases (forms/load-project-aliases to)
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
                         (keys (future-files result))))))))))

(defn- summary [result]
  (select-keys result [:moved-forms :public-forms :declared :requires-added :requires-removed
                       :qualifiers-rewritten :source-referred-forms :warnings
                       :callers-rewritten :callers-to-review]))

(defn plan-summary
  "The plan as the CLI prints it: no file contents."
  [opts]
  (let [result (plan opts)]
    (if (:ok result)
      (assoc (summary result) :ok true :file (:file opts) :to (:to opts))
      (dissoc result :source :target :callers))))

;; ============================================================
;; Apply, receipt, undo
;; ============================================================

(defn- write-receipt! [receipt-out entries]
  (when receipt-out
    (let [receipt {:kind :move-into
                   :files (mapv (fn [{:keys [path original result]}]
                                  {:path path
                                   :original original
                                   :result-hash (structural-lens/source-hash result)})
                                entries)}]
      (spit receipt-out (pr-str receipt))
      receipt-out)))

(defn- restore! [entries]
  (doseq [{:keys [path original]} entries]
    (file-ops/atomic-write! (io/file path) original))
  (every? #(= (:original %) (slurp (:path %))) entries))

(defn apply-plan!
  "Write a plan from `plan`. Every file must still match the hash it was
  planned from. A new clj-kondo error in any written file restores them all."
  [{:keys [file to receipt-out lint] :or {lint clj-kondo-lint}} result]
  (if-not (:ok result)
    (dissoc result :source :target :callers)
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
                               (new-errors (:findings baseline) (:findings after)))]
              (if (seq introduced)
                {:ok false
                 :error-type :lint-regression
                 :error "The move introduced clj-kondo errors; every file was restored"
                 :new-errors (vec (take 10 introduced))
                 :restored (restore! entries)}
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
               :restored (restore! entries)})))))))

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
    (if-not (= :move-into (:kind data))
      (extract/undo! opts)
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
