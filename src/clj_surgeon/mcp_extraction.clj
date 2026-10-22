(ns clj-surgeon.mcp-extraction
  "Pure compiler for one typed MCP namespace-extraction decision."
  (:require
   [clj-surgeon.extract :as extract]
   [clj-surgeon.file-ops :as file-ops]
   [clj-surgeon.intent-transaction :as transaction]
   [clj-surgeon.move-callers :as move-callers]
   [clj-surgeon.move-into :as move-into]
   [clj-surgeon.structural-lens :as structural-lens]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [rewrite-clj.parser :as parser]))

(def receipt-version 1)

(def caller-proof-levels
  {:semantic-complete
   {:scan-complete true
    :semantic-provider-used true
    :zero-callers-authoritative true
    :meaning "Resolved callers came from one hash-bound semantic session."}
   :structural-candidates-only
   {:scan-complete true
    :semantic-provider-used false
    :zero-callers-authoritative false
    :meaning "Exact structural scans completed; aliases, macros, or generated callers may remain."}
   :caller-proof-unavailable
   {:scan-complete false
    :semantic-provider-used false
    :zero-callers-authoritative false
    :meaning "No trustworthy caller inventory completed; absence must not authorize deletion."}})

(def structural-caller-proof
  (assoc (:structural-candidates-only caller-proof-levels)
         :level :structural-candidates-only))

(defn refusal
  [error-type message data]
  (merge
    {:ok false
     :error-type error-type
     :error message
     :source-unchanged true
     :target-unchanged true}
    data))

(defn normalize-mechanical-fields
  "Default omitted bookkeeping without erasing an omitted visibility decision."
  [{:keys [file to forms caller-changes ignored-caller-files expect]
    :as request}]
  (let [caller-changes (or caller-changes [])
        ignored-caller-files (or ignored-caller-files [])
        expect (or expect
                   {:forms (count forms)
                    :caller-edits
                    (reduce + (map #(get-in % [:expect :matches])
                                   caller-changes))
                    :files (count (distinct (concat [file to]
                                                    (mapcat :files
                                                            caller-changes))))})]
    (assoc request
           :caller-changes caller-changes
           :ignored-caller-files ignored-caller-files
           :expect expect)))

(defn validate-request
  [{:keys [file to forms public-forms require-policy expect caller-changes
           ignored-caller-files source-hash]
    :as request}]
  (cond
    (not (map? request))
    (refusal :invalid-extraction-request
             "extraction must be an object"
             {})

    (not (string? file))
    (refusal :invalid-extraction-file
             "extraction.file must be a project-relative source path"
             {})

    (not (string? to))
    (refusal :invalid-extraction-target
             "extraction.to must be a project-relative absent source path"
             {})

    (= file to)
    (refusal :extraction-path-collision
             "extraction.file and extraction.to must differ"
             {:file file})

    (or (not (vector? forms))
        (empty? forms)
        (not-every? string? forms)
        (not= (count forms) (count (distinct forms))))
    (refusal :invalid-extraction-forms
             "extraction.forms must contain distinct form names"
             {})

    (and (contains? request :public-forms)
         (or (not (vector? public-forms))
             (not-every? string? public-forms)
             (not= (count public-forms) (count (distinct public-forms)))))
    (refusal :invalid-public-forms
             "extraction.public_forms must contain distinct form names"
             {})

    ;; @spec MCP-OP-FIELD-002
    (not (#{:minimal :copy-all} require-policy))
    (refusal :invalid-require-policy
             "extraction.require_policy must be minimal or copy-all"
             {:field "extraction.require_policy"
              :path ["extraction" "require_policy"]
              :accepted ["minimal" "copy-all"]
              :actual (some-> require-policy name)
              :require-policy require-policy
              :remedy (str "extraction.require_policy is required and is never "
                           "defaulted. Send \"minimal\" to copy only the requires "
                           "the moved forms use, or \"copy-all\" to copy the "
                           "source namespace's complete require list, then call "
                           "apply_clojure_changes once.")})

    (and source-hash
         (or (not (string? source-hash))
             (not (re-matches #"[0-9a-f]{64}" source-hash))))
    (refusal :invalid-source-hash
             "extraction.source_hash must be a lowercase SHA-256 hex string"
             {})

    (not (map? expect))
    (refusal :invalid-extraction-expect
             "extraction.expect must be an object"
             {})

    (not= #{:forms :caller-edits :files} (set (keys expect)))
    (refusal :invalid-extraction-expect
             "extraction.expect requires exactly forms, caller_edits, and files"
             {:fields (vec (sort (keys expect)))})

    (not-every? nat-int? (vals expect))
    (refusal :invalid-extraction-expect
             "All extraction.expect counts must be non-negative integers"
             {:actual expect})

    (not (vector? caller-changes))
    (refusal :invalid-caller-changes
             "extraction.caller_changes must be an array"
             {})

    (or (not (vector? ignored-caller-files))
        (not-every? string? ignored-caller-files)
        (not= (count ignored-caller-files)
              (count (distinct ignored-caller-files))))
    (refusal :invalid-ignored-callers
             "extraction.ignored_caller_files must contain distinct paths"
             {})

    :else
    {:ok true}))

(defn- caller-files
  [changes]
  (->> changes (mapcat :in) set))

(defn- compile-callers
  [sources changes expected-edits]
  (if (empty? changes)
    (if (zero? expected-edits)
      {:ok true
       :match-count 0
       :original-sources {}
       :future-sources {}}
      (refusal :caller-edit-count-mismatch
               "No caller changes were supplied for expect.caller_edits"
               {:expected expected-edits :actual 0}))
    (transaction/compile-transaction
      sources
      {:changes changes
       :expect {:changes (count changes)
                :edits expected-edits
                :files (count (caller-files changes))}})))

;; ============================================================
;; The move: the source and destination futures and the caller proposal
;; ============================================================

(defn- compile-new-namespace
  "Plan an extraction into a new namespace. Returns the source and
  destination futures, the structural caller candidates, and the caller
  proposal, or a refusal."
  [{:keys [file to forms public-forms require-policy source target-ns workspace-sources]}
   public-forms-supplied? authority max-candidates]
  (let [plan (extract/compile-plan
               {:file file
                :source source
                :forms forms
                :public-forms (or public-forms [])
                :derive-required-public-forms (not public-forms-supplied?)
                :to to
                :target-ns target-ns
                :workspace-sources workspace-sources
                :require-policy require-policy})]
    (cond
      (:error plan)
      (refusal (or (:error-type plan) :extraction-plan-refused)
               (:error plan)
               (select-keys plan [:invalid-public-forms :unsupported-public-forms]))

      (and public-forms-supplied? (seq (:missing-required-public-forms plan)))
      (refusal :required-public-forms-missing
               "Every moved private form called by remaining source must be public"
               {:required-public-forms (:required-public-forms plan)
                :missing-public-forms (:missing-required-public-forms plan)})

      :else
      (let [proposal (extract/with-caller-rewrites
                       plan workspace-sources
                       {:caller-authority authority :max-candidates max-candidates})]
        (if (:error proposal)
          (refusal (:error-type proposal) (:error proposal)
                   (dissoc proposal :ok :error :error-type))
          (let [future (extract/compile-candidates
                         {:source source
                          :source-file file
                          :target-file to
                          :form-ranges (:_form-texts plan)
                          :target-source (:_new-file-content plan)
                          :target-ns target-ns
                          :target-alias (:target-alias plan)
                          :source-referred-forms (:_source-referred-forms plan)})]
            {:ok true
             :plan plan
             :form-count (:form-count plan)
             :forms (:forms-to-extract plan)
             :require-policy (:require-policy plan)
             :source-future (:source future)
             :target-future (:target future)
             :target-original nil
             :candidates (set/union (set (:callers-to-review plan))
                                    (set (map :file (:quoted-var-references plan))))
             :callers (:_callers proposal)
             :callers-proposed (:callers-proposed proposal)
             :caller-problems (:caller-problems proposal)
             :callers-to-review (:callers-to-review plan)
             :quoted-var-references (:quoted-var-references plan)}))))))

(defn- compile-existing-namespace
  "Plan a move into an existing namespace through the shared move planner."
  [{:keys [file to forms public-forms source target target-ns anchor workspace-sources
           project-aliases]}
   public-forms-supplied? authority max-candidates]
  (let [moved (move-into/compile-move
                (cond-> {:source-file file
                         :source source
                         :target-file to
                         :target target
                         :target-ns target-ns
                         :forms (mapv symbol forms)
                         :anchor anchor
                         :project-aliases (or project-aliases {})
                         :workspace-sources workspace-sources
                         :caller-authority authority
                         :max-candidates max-candidates}
                  public-forms-supplied? (assoc :public-forms (mapv symbol public-forms))))]
    (if-not (:ok moved)
      (refusal (or (:error-type moved) :move-refused)
               (or (:error moved) "The move refused")
               (dissoc moved :ok :error :error-type :source-unchanged :target-unchanged))
      (let [problem-files (set (map :file (:caller-problems moved)))]
        {:ok true
         :form-count (count (:moved-forms moved))
         :forms (:moved-forms moved)
         :require-policy :merged
         :source-future (:source moved)
         :target-future (:target moved)
         :target-original target
         :block (:block moved)
         :candidates (into (set (keys (:callers moved))) problem-files)
         :callers (:callers moved)
         :callers-proposed (:callers-rewritten moved)
         :caller-problems (:caller-problems moved)
         :callers-to-review (vec (sort (into (set (keys (:callers moved))) problem-files)))
         :quoted-var-references []
         :public-forms (:public-forms moved)
         :warnings (:warnings moved)}))))

;; @spec MCP-OP-PLAN-004
;; @spec MCP-OP-PLAN-007
;; @spec MCP-OP-PLAN-008
;; @spec MCP-OP-PLAN-009
;; @spec MCP-OP-PLAN-010
;; @spec MCP-OP-PLAN-011
;; @spec MCP-OP-PLAN-012
;; @spec MCP-OP-PLAN-013
(defn compile-extraction
  "Compile an extraction or a move into an existing namespace, and its
  callers, against one captured snapshot. The kernel proposes caller
  rewrites; it writes them only on callers :accept-proposed, and otherwise
  the request's own caller decisions must account for every candidate."
  [{:keys [file to source source-hash created-directories into-existing callers
           max-caller-candidates expect-derived]
    :or {created-directories []}
    :as request}]
  (let [public-forms-supplied? (contains? request :public-forms)
        request (normalize-mechanical-fields request)
        {:keys [expect caller-changes ignored-caller-files]} request
        validation (validate-request request)
        accept? (= :accept-proposed callers)
        authority (if accept? :kernel :model)
        expected-proposal (:callers-proposal-hash request)]
    (if-not (:ok validation)
      validation
      (if (and source-hash
               (not= source-hash (structural-lens/source-hash source)))
        (refusal :source-hash-mismatch
                 "The extraction source changed after planning"
                 {:expected source-hash
                  :actual (structural-lens/source-hash source)})
        (let [moved (if into-existing
                      (compile-existing-namespace request public-forms-supplied? authority
                                                  max-caller-candidates)
                      (compile-new-namespace request public-forms-supplied? authority
                                             max-caller-candidates))]
          (if-not (:ok moved)
            moved
            (try
              (let [workspace-sources (:workspace-sources request)
                    proposal (move-callers/proposal-hash (:callers moved) workspace-sources)
                    changed-caller-files (caller-files caller-changes)
                    forbidden (set/intersection #{file to} changed-caller-files)
                    accounted (set/union changed-caller-files (set ignored-caller-files))
                    omitted (if accept? #{} (set/difference (:candidates moved) accounted))
                    caller-sources (select-keys workspace-sources changed-caller-files)
                    caller-result (if accept?
                                    {:ok true
                                     :match-count (reduce + 0 (map :rewritten (:callers-proposed moved)))
                                     :original-sources (select-keys workspace-sources (keys (:callers moved)))
                                     :future-sources (:callers moved)}
                                    (compile-callers caller-sources caller-changes
                                                     (if expect-derived
                                                       (reduce + (map #(get-in % [:expect :matches]) caller-changes))
                                                       (:caller-edits expect))))
                    originals (cond-> (merge (sorted-map file source)
                                             (:original-sources caller-result))
                                (:target-original moved) (assoc to (:target-original moved)))
                    futures (merge (sorted-map file (:source-future moved)
                                               to (:target-future moved))
                                   (:future-sources caller-result))
                    actual-files (count futures)
                    expect (if expect-derived
                             {:forms (:form-count moved)
                              :caller-edits (:match-count caller-result)
                              :files actual-files}
                             expect)]
                (cond
                  (and accept? expected-proposal (not= expected-proposal proposal))
                  (refusal :stale-caller-proposal
                           "The callers changed since the proposal you accepted; nothing was written"
                           {:expected expected-proposal
                            :actual proposal
                            :callers-proposed (:callers-proposed moved)
                            :callers-proposal-hash proposal})

                  (not= (:forms expect) (:form-count moved))
                  (refusal :extraction-count-mismatch
                           "The compiled extraction form count did not match expect.forms"
                           {:expected (:forms expect)
                            :actual (:form-count moved)})

                  (seq forbidden)
                  (refusal :caller-change-path-collision
                           "caller_changes may not target extraction.file or extraction.to"
                           {:files (vec (sort forbidden))})

                  (seq omitted)
                  (let [unknowns
                        (mapv (fn [caller-file]
                                {:decision :caller-disposition
                                 :file caller-file
                                 :source-hash
                                 (some-> (get workspace-sources caller-file)
                                         structural-lens/source-hash)})
                              (sort omitted))]
                    (refusal
                      :extraction-decisions-required
                      "Caller candidates require an explicit change or ignore decision"
                      {:files (vec (sort omitted))
                       :mutation-attempted false
                       :write-authority false
                       :remedy
                       (str "Fill each caller disposition in next_call and call apply_clojure_changes once, "
                            "or send callers \"accept-proposed\" to write the proposed rewrites listed in callers_proposed.")
                       :genuine-unknowns unknowns
                       :callers-proposed (:callers-proposed moved)
                       :caller-problems (:caller-problems moved)
                       :callers-proposal-hash proposal
                       :completed-plan
                       {:file file
                        :to to
                        :forms-to-extract (:forms moved)
                        :form-count (:form-count moved)
                        :required-public-forms
                        (or (some-> moved :plan :required-public-forms) [])
                        :require-policy (:require-policy moved)
                        :source-hash (structural-lens/source-hash source)
                        :callers-to-review (:callers-to-review moved)
                        :quoted-var-references (:quoted-var-references moved)}}))

                  (not (:ok caller-result))
                  caller-result

                  (not= (:files expect) actual-files)
                  (refusal :extraction-file-count-mismatch
                           "The compiled file count did not match expect.files"
                           {:expected (:files expect) :actual actual-files})

                  :else
                  (cond->
                    {:ok true
                     :operation :compiled-extraction
                     :caller-proof structural-caller-proof
                     :file file
                     :to to
                     :forms (:forms moved)
                     :form-count (:form-count moved)
                     :caller-edit-count (:match-count caller-result)
                     :require-policy (:require-policy moved)
                     :callers-to-review (:callers-to-review moved)
                     :quoted-var-references (:quoted-var-references moved)
                     :original-sources originals
                     :future-sources futures
                     :created-directories (vec created-directories)
                     :created-files (if into-existing [] [to])}
                    accept? (assoc :callers-rewritten (:callers-proposed moved))
                    (:block moved) (assoc :moved-block (:block moved))
                    (seq (:warnings moved)) (assoc :warnings (:warnings moved)))))
              (catch Exception error
                (refusal :invalid-extraction-result
                         (.getMessage error)
                         {})))))))))

(defn with-future-sources
  "Replace a compiled extraction's candidates after a staged source
   transformation. Created-file and original-source ownership remain exact."
  [compiled future-sources]
  (try
    (when-not (and (:ok compiled)
                   (map? future-sources)
                   (= (set (keys (:future-sources compiled)))
                      (set (keys future-sources))))
      (throw (ex-info "Transformed extraction sources must cover the file set exactly"
                      {:error-type :invalid-future-sources})))
    (doseq [[file source] future-sources]
      (when-not (string? source)
        (throw (ex-info "Transformed extraction source must be text"
                        {:error-type :invalid-future-source :file file})))
      (try
        (parser/parse-string-all source)
        (catch Exception error
          (throw (ex-info "Transformed extraction source does not parse"
                          {:error-type :invalid-future-source
                           :file file
                           :cause-error (.getMessage error)})))))
    (assoc compiled :future-sources (into (sorted-map) future-sources))
    (catch clojure.lang.ExceptionInfo error
      (merge {:ok false :error (.getMessage error)} (ex-data error)))
    (catch Exception error
      {:ok false
       :error-type :future-source-transformation-failed
       :error (.getMessage error)})))

(defn- source-hash
  [source]
  (structural-lens/source-hash source))

(defn build-receipt
  [compiled]
  (let [receipt
        {:receipt-version receipt-version
         :operation :compiled-extraction
         :caller-proof (:caller-proof compiled)
         :created-directories (vec (:created-directories compiled))
         :files
         (mapv
           (fn [[file future]]
             (if-let [original (get (:original-sources compiled) file)]
               {:file file
                :source-hash (source-hash original)
                :result-hash (source-hash future)
                :original-source original
                :result-source future}
               {:file file
                :absent-before true
                :result-hash (source-hash future)
                :result-source future}))
           (:future-sources compiled))
         :inverse {:operation :undo-compiled-extraction}}]
    (assoc receipt :receipt-hash (source-hash (pr-str receipt)))))

(defn- file-state
  [{:keys [read-source exists?]} file]
  (when (exists? file)
    (read-source file)))

(defn- rollback-file!
  [{:keys [read-source write-source! delete-file! exists?] :as io}
   originals futures created-files file]
  (let [current (file-state io file)
        future (get futures file)]
    (cond
      (contains? created-files file)
      (cond
        (nil? current) {:file file :recovered true :state :absent}
        (= current future) (do (delete-file! file)
                               {:file file :recovered (not (exists? file))
                                :state :deleted})
        :else {:file file :recovered false :state :unknown-bytes})

      (= current (get originals file))
      {:file file :recovered true :state :original}

      (= current future)
      (do (write-source! file (get originals file))
          {:file file
           :recovered (= (get originals file) (read-source file))
           :state :restored})

      :else
      {:file file :recovered false :state :unknown-bytes})))

(defn- rollback-created-directories!
  [{:keys [exists? delete-file!]} directories]
  (mapv
    (fn [directory]
      (if-not (exists? directory)
        {:directory directory :recovered true :state :absent}
        (try
          (delete-file! directory)
          {:directory directory
           :recovered (not (exists? directory))
           :state :deleted}
          (catch Exception error
            {:directory directory
             :recovered false
             :state :not-empty-or-changed
             :cause-error (.getMessage error)}))))
    (reverse directories)))

(defn commit!
  "Commit one compiled mixed create/update file set through injected I/O."
  ([compiled]
   (commit! compiled
            {:read-source slurp
             :write-source! file-ops/atomic-write!
             :exists? #(.exists (io/file %))
             :create-directory!
             #(java.nio.file.Files/createDirectory
                (.toPath (io/file %))
                (make-array java.nio.file.attribute.FileAttribute 0))
             :delete-file! #(java.nio.file.Files/delete (.toPath (io/file %)))}))
  ([compiled {:keys [read-source write-source! exists? create-directory!] :as io}]
   (try
     (let [created-files (set (:created-files compiled))
           planned-directories (vec (:created-directories compiled))
           created-directories (atom [])
           originals (:original-sources compiled)
           futures (:future-sources compiled)
           ordered-files (vec (concat (keys originals) created-files))]
       (when-not (:ok compiled)
         (throw (ex-info "Commit requires a successful compiled extraction"
                         {:error-type :invalid-compiled-extraction})))
       (doseq [[file original] originals]
         (when-not (and (exists? file) (= original (read-source file)))
           (throw (ex-info "Extraction source changed before commit"
                           {:error-type :source-hash-mismatch :file file}))))
       (doseq [file created-files]
         (when (exists? file)
           (throw (ex-info "Extraction target appeared before commit"
                           {:error-type :target-already-exists :file file}))))
       (doseq [directory planned-directories]
         (when (exists? directory)
           (throw (ex-info "Extraction target parent appeared before commit"
                           {:error-type :target-parent-state-changed
                            :directory directory}))))
       (try
         (doseq [directory planned-directories]
           (when (exists? directory)
             (throw (ex-info "Extraction target parent appeared during commit"
                             {:error-type :target-parent-state-changed
                              :directory directory})))
           (create-directory! directory)
           (swap! created-directories conj directory)
           (when-not (exists? directory)
             (throw (ex-info "Extraction target parent creation could not be verified"
                             {:error-type :target-parent-create-failed
                              :directory directory}))))
         (doseq [file ordered-files]
           (if (contains? created-files file)
             (when (exists? file)
               (throw (ex-info "Extraction target appeared during commit"
                               {:error-type :target-already-exists :file file})))
             (when-not (= (get originals file) (read-source file))
               (throw (ex-info "Extraction source changed during commit"
                               {:error-type :source-hash-mismatch :file file}))))
           (write-source! file (get futures file))
           (when-not (= (get futures file) (read-source file))
             (throw (ex-info "Extraction read-back verification failed"
                             {:error-type :read-back-hash-mismatch :file file}))))
         (let [receipt (build-receipt
                         (assoc compiled
                                :created-directories @created-directories))]
           {:ok true
            :operation :compiled-extraction
            :committed true
            :change-count 1
            :match-count (+ (:form-count compiled)
                            (:caller-edit-count compiled))
            :changed-file-count (count ordered-files)
            :caller-proof (:caller-proof compiled)
            :format (:format compiled)
            :receipt receipt
            :receipt-hash (:receipt-hash receipt)
            :verified {:whole-files true
                       :file-count (count ordered-files)
                       :read-back true
                       :read-back-hashes
                       (into (sorted-map)
                             (map (fn [[file source]]
                                    [file (source-hash source)]))
                             futures)}})
         (catch Exception cause
           (let [file-recovery (mapv #(rollback-file! io originals futures
                                                      created-files %)
                                     (reverse ordered-files))
                 directory-recovery
                 (rollback-created-directories! io @created-directories)
                 recovery (into file-recovery directory-recovery)
                 rolled-back (every? :recovered recovery)]
             {:ok false
              :error-type (if rolled-back
                            :extraction-write-failed
                            :extraction-recovery-required)
              :error (if rolled-back
                       "Extraction write failed; all files restored"
                       "Extraction write failed; manual recovery required")
              :cause-error (.getMessage cause)
              :cause-error-type (:error-type (ex-data cause))
              :rolled-back rolled-back
              :recovery recovery}))))
     (catch clojure.lang.ExceptionInfo error
       (merge {:ok false :error (.getMessage error)} (ex-data error)))
     (catch Exception error
       {:ok false
        :error-type :extraction-write-exception
        :error (.getMessage error)}))))

(defn undo!
  "Undo a compiled extraction receipt through the same guarded file-set rules."
  ([receipt]
   (undo! receipt
          {:read-source slurp
           :write-source! file-ops/atomic-write!
           :exists? #(.exists (io/file %))
           :create-directory!
           #(java.nio.file.Files/createDirectory
              (.toPath (io/file %))
              (make-array java.nio.file.attribute.FileAttribute 0))
           :delete-file! #(java.nio.file.Files/delete (.toPath (io/file %)))}))
  ([receipt {:keys [read-source write-source! exists? delete-file!
                    create-directory!] :as io}]
   (try
     (when-not (and (= receipt-version (:receipt-version receipt))
                    (= :compiled-extraction (:operation receipt))
                    (vector? (:files receipt)))
       (throw (ex-info "Invalid compiled extraction receipt"
                       {:error-type :invalid-extraction-receipt})))
     (doseq [{:keys [file result-source]} (:files receipt)]
       (when-not (and (exists? file) (= result-source (read-source file)))
         (throw (ex-info "Extraction result changed before undo"
                         {:error-type :stale-extraction-result :file file}))))
     (try
       (doseq [{:keys [file absent-before original-source]} (:files receipt)]
         (if absent-before
           (delete-file! file)
           (write-source! file original-source)))
       (let [directory-recovery
             (rollback-created-directories!
               io (:created-directories receipt))
             verified?
             (and
               (every?
                 (fn [{:keys [file absent-before original-source]}]
                   (if absent-before
                     (not (exists? file))
                     (and (exists? file) (= original-source (read-source file)))))
                 (:files receipt))
               (every? :recovered directory-recovery))]
         (if verified?
           {:ok true
            :operation :undo-compiled-extraction
            :verified {:whole-files true
                       :file-count (count (:files receipt))
                       :read-back true}}
           (throw (ex-info "Extraction undo read-back verification failed"
                           {:error-type :extraction-undo-read-back-failed}))))
       (catch Exception cause
         (let [directory-recovery
               (mapv
                 (fn [directory]
                   (if (exists? directory)
                     {:directory directory :recovered true :state :present}
                     (try
                       (create-directory! directory)
                       {:directory directory
                        :recovered (exists? directory)
                        :state :recreated}
                       (catch Exception error
                         {:directory directory
                          :recovered false
                          :state :recreate-failed
                          :cause-error (.getMessage error)}))))
                 (:created-directories receipt))
               recovery
               (mapv
                 (fn [{:keys [file result-source]}]
                   (let [current (file-state io file)]
                     (cond
                       (= current result-source)
                       {:file file :recovered true :state :result}

                       (or (nil? current)
                           (= current (:original-source
                                        (first (filter #(= file (:file %))
                                                       (:files receipt))))))
                       (do (write-source! file result-source)
                           {:file file
                            :recovered (= result-source (read-source file))
                            :state :restored-result})

                       :else
                       {:file file :recovered false :state :unknown-bytes})))
                 (reverse (:files receipt)))
               recovery (into directory-recovery recovery)
               recovered (every? :recovered recovery)]
           {:ok false
            :error-type (if recovered
                          :extraction-undo-failed
                          :extraction-undo-recovery-required)
            :error (if recovered
                     "Extraction undo failed; transaction result restored"
                     "Extraction undo failed; manual recovery required")
            :cause-error (.getMessage cause)
            :rolled-back recovered
            :recovery recovery})))
     (catch clojure.lang.ExceptionInfo error
       (merge {:ok false :error (.getMessage error)} (ex-data error)))
     (catch Exception error
       {:ok false
        :error-type :extraction-undo-exception
        :error (.getMessage error)}))))
