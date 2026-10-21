(ns clj-surgeon.move-into-shell
  "File boundary for moving forms into an existing namespace
  (docs/plans/move-into-existing-namespace.md). Reads one snapshot of every
  file, delegates every decision to the pure clj-surgeon.move-into planner,
  then writes both files atomically, lints them, and restores both on a new
  clj-kondo error. Receipts keep the original bytes of both files."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clj-surgeon.diagnostic-delta :as diagnostic-delta]
   [clj-surgeon.extract :as extract]
   [clj-surgeon.file-ops :as file-ops]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.move-into :as move-into]
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

(defn- workspace-sources [file]
  (let [root (or (some->> (iterate #(some-> % .getParentFile)
                                   (.getCanonicalFile (io/file file)))
                          (take-while some?)
                          (filter #(.exists (io/file % "deps.edn")))
                          first)
                 (.getParentFile (.getCanonicalFile (io/file file))))]
    (->> (file-seq root)
         (filter #(.isFile %))
         (filter #(re-matches #".*\.clj[sc]?$" (.getName %)))
         (remove #(str/includes? (.getPath %) "/.git/"))
         (map (fn [f] [(.getPath f) (slurp f)]))
         (into (sorted-map)))))

(defn- target-namespace [target]
  (some-> (move-into/ns-form-source target)
          (str/replace #"^\(ns\s+" "")
          (str/split #"[\s)]" 2)
          first))

(defn plan
  "Plan a move into an existing namespace without writing. The result names
  every consequence but carries no file contents."
  [{:keys [file to forms] :as opts}]
  (let [target-file (io/file to)]
    (if-not (.exists target-file)
      {:ok false
       :error-type :into-existing-target-missing
       :error "--into-existing needs a target file that exists"
       :to to
       :next-step "Omit --into-existing to create a new namespace file"
       :source-unchanged true
       :target-unchanged true}
      (let [source (slurp file)
            target (slurp target-file)
            sources (workspace-sources file)
            result (move-into/compile-move
                     (cond-> {:source-file (.getPath (io/file file))
                              :source source
                              :target-file (.getPath target-file)
                              :target target
                              :target-ns (target-namespace target)
                              :forms forms
                              :anchor (select-keys opts anchor-option-keys)
                              :project-aliases (forms/load-project-aliases to)
                              :workspace-sources sources}
                       (contains? opts :public-forms)
                       (assoc :public-forms (:public-forms opts))))]
        (assoc result
               :snapshot {:source-hash (structural-lens/source-hash source)
                          :target-hash (structural-lens/source-hash target)})))))

(defn- summary [result]
  (select-keys result [:moved-forms :public-forms :declared :requires-added
                       :qualifiers-rewritten :source-referred-forms :warnings
                       :callers-to-review :quoted-var-references]))

(defn plan-summary
  "The plan as the CLI prints it: no file contents."
  [opts]
  (let [result (plan opts)]
    (if (:ok result)
      (assoc (summary result) :ok true :file (:file opts) :to (:to opts))
      (dissoc result :source :target))))

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

(defn execute!
  "Apply a move into an existing namespace. Refusals leave both files
  unchanged; a new clj-kondo error after the write restores both."
  [{:keys [file to receipt-out lint] :or {lint clj-kondo-lint} :as opts}]
  (let [result (plan opts)]
    (if-not (:ok result)
      (dissoc result :source :target)
      (let [source-file (io/file file)
            target-file (io/file to)
            original-source (slurp source-file)
            original-target (slurp target-file)
            entries [{:path (.getPath source-file) :original original-source :result (:source result)}
                     {:path (.getPath target-file) :original original-target :result (:target result)}]]
        (if (not= [(structural-lens/source-hash original-source)
                   (structural-lens/source-hash original-target)]
                  [(get-in result [:snapshot :source-hash])
                   (get-in result [:snapshot :target-hash])])
          {:ok false
           :error-type :stale-source
           :error "A file changed after planning"
           :source-unchanged true
           :target-unchanged true}
          (let [files [source-file target-file]
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
                   :error "The move introduced clj-kondo errors; both files were restored"
                   :new-errors (vec (take 10 introduced))
                   :restored (restore! entries)}
                  (cond-> (assoc (summary result)
                                 :ok true
                                 :applied true
                                 :file file
                                 :to to
                                 :lint (if (:checked after)
                                         {:checked true :new-errors 0}
                                         {:checked false :reason (:reason after)}))
                    receipt-out (assoc :receipt-file (write-receipt! receipt-out entries)))))
              (catch Exception error
                {:ok false
                 :error-type (or (:error-type (ex-data error)) :move-write-failed)
                 :error (.getMessage error)
                 :restored (restore! entries)}))))))))

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
