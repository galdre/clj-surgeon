(ns clj-surgeon.mcp-extraction-plan
  "Bounded, non-mutating extraction planning over the production compiler."
  (:require
   [clj-surgeon.extract :as extract]
   [clj-surgeon.forms :as forms]
   [clj-surgeon.mcp-paths :as mcp-paths]
   [clj-surgeon.mcp-workspace-sources :as workspace-sources]
   [clj-surgeon.move-callers :as move-callers]
   [clj-surgeon.move-into :as move-into]
   [clj-surgeon.structural-lens :as structural-lens]
   [clojure.string :as str]))

(def private-plan-fields
  #{:_form-texts :_new-file-content :_source :_source-hash
    :_source-referred-forms :_callers})

(def allowed-request-fields
  #{:workspace_root :mode :file :to :forms :require_policy
    :into_existing :above :below :at_start :at_end :source_roots})

(def ^:private anchor-fields [:above :below :at_start :at_end])

(defn- refusal
  [error-type error & [data]]
  (merge {:ok false
          :operation "inspect_clojure"
          :mode "plan-extraction"
          :error_type (name error-type)
          :error error
          :read_complete false
          :source_unchanged true
          :next_action "correct_request"}
         data))

(defn validate-request
  [params]
  (let [unknown (vec (sort (remove allowed-request-fields (keys params))))]
    (cond
      (seq unknown)
      (refusal :unknown-fields "Extraction plan contains unknown fields"
               {:unknown_fields unknown})

      (not= "plan-extraction" (:mode params))
      (refusal :invalid-mode "mode must be plan-extraction")

      (not (string? (:file params)))
      (refusal :invalid-extraction-file "file must be a project-relative source path")

      (not (string? (:to params)))
      (refusal :invalid-extraction-target "to must be a project-relative absent source path")

      (or (not (vector? (:forms params)))
          (empty? (:forms params))
          (not-every? string? (:forms params))
          (not= (count (:forms params)) (count (distinct (:forms params)))))
      (refusal :invalid-extraction-forms "forms must contain distinct form names")

      ;; @spec MCP-OP-FIELD-002
      (not (#{"minimal" "copy-all"} (:require_policy params)))
      (refusal :invalid-require-policy
               "require_policy must be minimal or copy-all"
               {:field "require_policy"
                :accepted ["minimal" "copy-all"]
                :actual (:require_policy params)
                :remedy (str "require_policy is required and is never defaulted. "
                             "Send \"minimal\" to copy only the requires the moved "
                             "forms use, or \"copy-all\" to copy the source "
                             "namespace's complete require list, then call "
                             "inspect_clojure once.")})

      (and (some #(contains? params %) anchor-fields) (not (true? (:into_existing params))))
      (refusal :anchor-without-into-existing
               "above, below, at_start and at_end place forms in an existing namespace; set into_existing")

      (and (contains? params :source_roots)
           (not (and (vector? (:source_roots params))
                     (every? #(and (string? %) (not (str/blank? %))
                                   (not (.isAbsolute (java.io.File. ^String %)))
                                   (not-any? #{".."} (str/split % #"[/\\\\]")))
                             (:source_roots params)))))
      (refusal :invalid-source-roots
               "source_roots takes directories relative to the workspace, inside it")

      :else
      {:ok true})))

(defn- public-reference
  [relative-paths reference]
  (cond-> reference
    (:file reference) (update :file #(get relative-paths % %))))

(defn- public-proposal
  [relative-paths entries]
  (mapv #(public-reference relative-paths %) entries))

;; @spec MCP-OP-PLAN-011
;; @spec MCP-OP-PLAN-012
(defn- compile-move-plan-response
  [{:keys [workspace-root file to source-path target-path source target target-ns
           workspace-sources relative-paths require-policy forms anchor source-roots
           max-candidates]}]
  (let [moved (move-into/compile-move
                {:source-file source-path :source source
                 :target-file target-path :target target :target-ns target-ns
                 :forms (mapv symbol forms) :anchor anchor
                 :project-aliases (forms/load-project-aliases target-path workspace-root)
                 :workspace-sources workspace-sources
                 :caller-authority :model
                 :max-candidates max-candidates})]
    (if-not (:ok moved)
      (refusal (or (:error-type moved) :move-refused) (or (:error moved) "The move refused")
               {:refusal (dissoc moved :ok :error :error-type :source :target :callers :block)})
      (let [proposed (public-proposal relative-paths (:callers-rewritten moved))
            problems (public-proposal relative-paths (:caller-problems moved))
            source-hash (structural-lens/source-hash source)]
        {:ok true
         :operation "inspect_clojure"
         :mode "plan-extraction"
         :plan {:file file :to to :into-existing true
                :moved-forms (:moved-forms moved)
                :public-forms (:public-forms moved)
                :requires-added (:requires-added moved)
                :requires-removed (:requires-removed moved)
                :warnings (:warnings moved)
                :callers-proposed proposed
                :caller-problems problems
                :callers-proposal-hash (move-callers/proposal-hash (:callers moved) workspace-sources)}
         :source_hash source-hash
         :next_call
         {:workspace_root workspace-root
          :extraction
          (merge {:file file :to to :forms forms
                  :require_policy (name require-policy)
                  :into_existing true
                  :source_hash source-hash}
                 (when source-roots {:source_roots source-roots})
                 (into {} (map (fn [[k v]] [(keyword (str/replace (name k) "-" "_")) v])) anchor))}
         :read_complete true
         :source_unchanged true
         :next_action (if (seq proposed)
                        "decide_callers_then_apply_once"
                        "apply_once")}))))

;; @spec MCP-OP-PLAN-001
;; @spec MCP-OP-PLAN-002
;; @spec MCP-OP-PLAN-003
;; @spec MCP-OP-PLAN-012
(defn compile-plan-response
  [{:keys [workspace-root file to source-path target-path source forms target-ns
           workspace-sources relative-paths require-policy source-roots max-candidates]}]
  (try
    (let [compiled
          (extract/compile-plan
            {:file source-path
             :source source
             :forms forms
             :public-forms []
             :to target-path
             :target-ns target-ns
             :workspace-sources workspace-sources
             :require-policy require-policy})
          proposal (when-not (:error compiled)
                     (extract/with-caller-rewrites compiled workspace-sources
                       {:caller-authority :model :max-candidates max-candidates}))]
      (cond
        (:error compiled)
        (refusal :extraction-plan-refused (:error compiled))

        (:error proposal)
        (refusal (or (:error-type proposal) :extraction-plan-refused) (:error proposal))

        :else
        (let [callers (mapv #(get relative-paths % %) (:callers-to-review compiled))
              quoted (mapv #(public-reference relative-paths %)
                           (:quoted-var-references compiled))
              source-hash (:_source-hash compiled)
              public-plan (-> (apply dissoc compiled private-plan-fields)
                              (assoc :file file
                                     :to to
                                     :callers-to-review callers
                                     :quoted-var-references quoted
                                     :callers-proposed (public-proposal relative-paths (:callers-proposed proposal))
                                     :caller-problems (public-proposal relative-paths (:caller-problems proposal))
                                     :callers-proposal-hash (move-callers/proposal-hash (:_callers proposal) workspace-sources)))]
          {:ok true
           :operation "inspect_clojure"
           :mode "plan-extraction"
           :plan public-plan
           :source_hash source-hash
           :evidence_counts
           {:caller_candidates {:returned (count callers)
                                :omitted 0
                                :truncated false}
            :quoted_var_references {:returned (count quoted)
                                    :omitted 0
                                    :truncated false}}
           :next_call
           {:workspace_root workspace-root
            :extraction
            (cond-> {:file file
                     :to to
                     :forms forms
                     :public_forms (:required-public-forms compiled)
                     :require_policy (name require-policy)
                     :source_hash source-hash
                     :caller_changes []
                     :ignored_caller_files []}
              source-roots (assoc :source_roots source-roots))}
           :read_complete true
           :source_unchanged true
           :next_action "fill_caller_decisions_then_apply_once"})))
    (catch Exception error
      (refusal :extraction-plan-failed (.getMessage error)))))

(defn plan!
  [{:keys [project-root max-caller-candidates max-workspace-files max-workspace-bytes]} params]
  (let [validation (validate-request params)]
    (if-not (:ok validation)
      validation
      (let [root (mcp-paths/real-root project-root)
            into-existing (true? (:into_existing params))
            source-result (mcp-paths/resolve-source-path root (:file params))
            target-result (if into-existing
                            (mcp-paths/resolve-source-path root (:to params))
                            (mcp-paths/resolve-new-source-path root (:to params)))
            path-refusal (first (remove :ok [source-result target-result]))]
        (if path-refusal
          (merge path-refusal
                 {:operation "inspect_clojure"
                  :mode "plan-extraction"
                  :read_complete false
                  :source_unchanged true
                  :next_action "correct_request"})
          (let [source-path (str (:path source-result))
                target-path (str (:path target-result))
                ;; @spec MCP-OP-PLAN-014
                declared (workspace-sources/read-declared
                           root (:source_roots params)
                           {:max-files (or max-workspace-files 20000)
                            :max-bytes (or max-workspace-bytes (* 256 1024 1024))})
                sources (cond-> declared
                          true (assoc source-path (slurp source-path))
                          into-existing (assoc target-path (slurp target-path)))]
            (if-not (sorted? declared)
              (refusal (:error-type declared) (:error declared)
                       (select-keys declared [:files :bytes :limit :source-roots :next-step]))
              (if into-existing
                (compile-move-plan-response
                  {:workspace-root (str root)
                   :file (:file params)
                   :to (:to params)
                   :source-path source-path
                   :target-path target-path
                   :source (get sources source-path)
                   :target (get sources target-path)
                   :target-ns (some-> (move-into/ns-form-source (get sources target-path))
                                      (str/replace #"^\(ns\s+" "")
                                      (str/split #"[\s)]" 2)
                                      first)
                   :forms (:forms params)
                   :anchor (into {} (keep (fn [k] (when (contains? params k)
                                                    [(keyword (str/replace (name k) "_" "-")) (get params k)])))
                                 anchor-fields)
                   :workspace-sources sources
                   :relative-paths (workspace-sources/relative-paths root sources)
                   :require-policy (keyword (:require_policy params))
                   :source-roots (:source_roots params)
                   :max-candidates (or max-caller-candidates 1000)})
                (compile-plan-response
                  {:workspace-root (str root)
                   :file (:file params)
                   :to (:to params)
                   :source-path source-path
                   :target-path target-path
                   :source (get sources source-path)
                   :forms (:forms params)
                   :target-ns (extract/file-path->ns-name
                                target-path ["src" "test" "dev"])
                   :workspace-sources sources
                   :relative-paths (workspace-sources/relative-paths root sources)
                   :require-policy (keyword (:require_policy params))
                   :source-roots (:source_roots params)
                   :max-candidates (or max-caller-candidates 1000)})))))))))
