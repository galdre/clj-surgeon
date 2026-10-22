(ns clj-surgeon.mcp-move-into-test
  "The MCP extraction boundary for moving into an existing namespace and for
  proposed caller rewrites (docs/intent/mcp-operation-contract/
  mcp-operation-contract-design.md, \"Moving into an existing namespace and
  proposed caller rewrites\"). Public requests against temporary workspaces;
  the formatter and linter are stubbed through config."
  (:require
   [clj-surgeon.extract :as extract]
   [clj-surgeon.mcp-extraction-plan :as extraction-plan]
   [clj-surgeon.mcp-tool :as mcp-tool]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "clj-surgeon-mcp-move-into-" (make-array FileAttribute 0))))

(defn- delete-tree! [file]
  (when (.exists (io/file file))
    (doseq [child (reverse (file-seq (io/file file)))]
      (Files/deleteIfExists (.toPath child)))))

(def ^:private invoice
  (str "(ns billing.invoice\n"
       "  (:require\n"
       "   [billing.customer :as customer]))\n\n"
       "(defn has-email? [c] (some? (customer/email c)))\n\n"
       "(defn deliver! [c] (when (has-email? c) :queued))\n"))

(def ^:private customer
  "(ns billing.customer)\n\n(defn email [c] (:email c))\n")

(def ^:private report
  (str "(ns billing.report\n"
       "  (:require\n"
       "   [billing.invoice :as inv]))\n\n"
       "(defn row [c] [(inv/has-email? c) (inv/deliver! c)])\n"))

(defn- workspace! []
  (let [root (temp-dir)]
    (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
    (doseq [[path text] {"src/billing/invoice.clj" invoice
                         "src/billing/customer.clj" customer
                         "src/billing/report.clj" report}]
      (let [f (io/file root path)]
        (.mkdirs (.getParentFile f))
        (spit f text)))
    root))

(defn- config [root & {:as extra}]
  (merge {:project-root (.getPath root)
          :receipt-dir (.getPath (io/file root "receipts"))
          :lint! (fn [_root _files] {:checked false :reason "stubbed"})}
         extra))

(defn- move-request [& {:as extra}]
  {:extraction (merge {:file "src/billing/invoice.clj"
                       :to "src/billing/customer.clj"
                       :forms ["has-email?"]
                       :require_policy "minimal"
                       :into_existing true
                       :below "email"}
                      extra)})

(defn- texts [root]
  (mapv #(slurp (io/file root %))
        ["src/billing/invoice.clj" "src/billing/customer.clj" "src/billing/report.clj"]))

;; @spec MCP-OP-PLAN-011
;; @spec MCP-OP-PLAN-013
(deftest an-accepted-move-writes-the-forms-and-the-callers-in-one-transaction
  (let [root (workspace!)
        before (texts root)]
    (try
      (let [result (mcp-tool/execute-request! (config root)
                                              (move-request :callers "accept-proposed"))]
        (is (:ok result) (pr-str result))
        (is (= 3 (:files result)))
        (is (str/includes? (slurp (io/file root "src/billing/customer.clj"))
                           "(defn email [c] (:email c))\n\n(defn has-email? [c] (some? (email c)))\n"))
        (is (str/includes? (slurp (io/file root "src/billing/report.clj"))
                           "[(customer/has-email? c) (inv/deliver! c)]"))
        (testing "the receipt undoes every file"
          (is (:ok (extract/undo! {:receipt (:undo_receipt result)})))
          (is (= before (texts root)))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-011
(deftest a-move-refuses-a-missing-anchor-or-target-before-writing
  (let [root (workspace!)
        before (texts root)]
    (try
      (is (= "anchor-required"
             (:error_type (mcp-tool/execute-request!
                            (config root) (update (move-request :callers "accept-proposed")
                                                  :extraction dissoc :below)))))
      (is (= "into-existing-target-missing"
             (:error_type (mcp-tool/execute-request!
                            (config root) (move-request :callers "accept-proposed"
                                                        :to "src/billing/absent.clj")))))
      (is (= before (texts root)))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-012
(deftest a-move-without-a-caller-decision-refuses-with-the-proposal
  (let [root (workspace!)
        before (texts root)]
    (try
      (let [result (mcp-tool/execute-request! (config root) (move-request))]
        (is (= "extraction-decisions-required" (:error_type result)))
        (is (= [{:file "src/billing/report.clj" :rewritten 1
                 :requires_added ["billing.customer"] :requires_removed []}]
               (:callers_proposed result)))
        (is (true? (:source_unchanged result)))
        (is (= before (texts root))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-012
;; @spec MCP-OP-PLAN-013
(deftest model-decisions-still-work-and-acceptance-cannot-mix-with-them
  (let [root (workspace!)]
    (try
      (testing "the model's own caller change is written; the proposal is not"
        (let [result (mcp-tool/execute-request!
                       (config root)
                       (move-request :caller_changes
                                     [{:id "redirect"
                                       :files ["src/billing/report.clj"]
                                       :forms ["row"]
                                       :find "inv/has-email?"
                                       :replace "billing.customer/has-email?"
                                       :expect {:matches 1 :each_form 1 :each_file 1}}]))]
          (is (:ok result) (pr-str result))
          (is (str/includes? (slurp (io/file root "src/billing/report.clj"))
                             "[(billing.customer/has-email? c) (inv/deliver! c)]"))
          (is (:ok (extract/undo! {:receipt (:undo_receipt result)})))))
      (testing "acceptance combined with model decisions refuses"
        (is (= "conflicting-caller-decisions"
               (:error_type (mcp-tool/execute-request!
                              (config root) (move-request :callers "accept-proposed"
                                                          :ignored_caller_files ["src/billing/report.clj"]))))))
      (testing "an unknown decision refuses"
        (is (= "invalid-callers-decision"
               (:error_type (mcp-tool/execute-request!
                              (config root) (move-request :callers "rewrite-everything"))))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-013
(deftest an-accepted-proposal-with-an-unrewritable-reference-refuses
  (let [root (workspace!)]
    (try
      (spit (io/file root "src/billing/dev.clj")
            "(ns billing.dev)\n\n(defn go [] (require 'billing.invoice) ((resolve 'billing.invoice/has-email?) {}))\n")
      (let [before (texts root)
            result (mcp-tool/execute-request! (config root) (move-request :callers "accept-proposed"))]
        (is (= "caller-not-rewritable" (:error_type result)))
        (is (= ["src/billing/dev.clj"] (mapv :file (:references result))))
        (is (= before (texts root))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-014
(deftest the-caller-scan-is-bounded-and-confined
  (let [root (workspace!)]
    (try
      (let [result (mcp-tool/execute-request! (config root :max-caller-candidates 0)
                                              (move-request :callers "accept-proposed"))]
        (is (= "caller-scan-budget-exceeded" (:error_type result)))
        (is (= 1 (:candidates result))))
      (testing "the walk's file and byte bounds apply before any file is read"
        (is (= "workspace-scan-budget-exceeded"
               (:error_type (mcp-tool/execute-request! (config root :max-workspace-files 2)
                                                       (move-request :callers "accept-proposed")))))
        (is (= "workspace-scan-budget-exceeded"
               (:error_type (mcp-tool/execute-request! (config root :max-workspace-bytes 100)
                                                       (move-request :callers "accept-proposed"))))))
      (doseq [bad [["../elsewhere"] ["/etc"]]]
        (is (= "invalid-source-roots"
               (:error_type (mcp-tool/execute-request!
                              (config root) (move-request :callers "accept-proposed" :source_roots bad))))
            (pr-str bad)))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-015
(deftest a-new-unresolved-reference-after-the-write-undoes-everything
  (let [root (workspace!)
        before (texts root)
        calls (atom 0)
        lint (fn [_root _files]
               (if (= 1 (swap! calls inc))
                 {:checked true :findings []}
                 {:checked true :findings [{:filename "src/billing/report.clj" :row 4
                                            :type :unresolved-var :level :warning
                                            :message "Unresolved var: inv/has-email?"}]}))]
    (try
      (let [result (mcp-tool/execute-request! (config root :lint! lint)
                                              (move-request :callers "accept-proposed"))]
        (is (= "lint-regression" (:error_type result)))
        (is (true? (:source_unchanged result)))
        (is (= before (texts root))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-016
(deftest the-formatter-formats-only-the-moved-block
  (let [root (workspace!)
        seen (atom nil)
        format! (fn [_root _command sources]
                  (reset! seen sources)
                  {:ok true
                   :future-sources (into {} (map (fn [[k v]] [k (str/replace v "(defn has-email? [c] " "(defn has-email?\n  [c]\n  ")]))
                                         sources)})]
    (try
      (spit (io/file root "src/billing/customer.clj")
            "(ns billing.customer)\n\n(defn   email [c]   (:email c))\n")
      (let [result (mcp-tool/execute-request! (config root :formatter ["fmt" "{files}"]
                                                      :format-candidates! format!)
                                              (move-request :callers "accept-proposed"))
            target (slurp (io/file root "src/billing/customer.clj"))]
        (is (:ok result) (pr-str result))
        (is (= 1 (count @seen)))
        (is (= "(defn has-email? [c] (some? (email c)))" (str/trim (val (first @seen)))))
        (is (str/includes? target "(defn   email [c]   (:email c))"))
        (is (str/includes? target "(defn has-email?\n  [c]\n  (some? (email c)))")))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-012
(deftest planning-publishes-the-proposal-for-a-move
  (let [root (workspace!)]
    (try
      (let [plan (extraction-plan/plan!
                   {:project-root (.getPath root)}
                   {:mode "plan-extraction"
                    :file "src/billing/invoice.clj"
                    :to "src/billing/customer.clj"
                    :forms ["has-email?"]
                    :require_policy "minimal"
                    :into_existing true
                    :below "email"})]
        (is (:ok plan) (pr-str plan))
        (is (= ["src/billing/report.clj"] (mapv :file (get-in plan [:plan :callers-proposed]))))
        (is (= [] (get-in plan [:plan :caller-problems])))
        (is (true? (get-in plan [:next_call :extraction :into_existing])))
        (is (= "email" (get-in plan [:next_call :extraction :below]))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-015
;; Fence review 2026-10-21 (F5): a lint that could not finish after the write
;; kept the write.
(deftest a-post-write-lint-that-cannot-finish-undoes-everything
  (let [root (workspace!)
        before (texts root)
        calls (atom 0)
        lint (fn [_root _files]
               (if (= 1 (swap! calls inc))
                 {:checked true :findings []}
                 {:checked false :reason "clj-kondo-admission-timeout"}))]
    (try
      (let [result (mcp-tool/execute-request! (config root :lint! lint)
                                              (move-request :callers "accept-proposed"))]
        (is (= "lint-unverified" (:error_type result)))
        (is (true? (:source_unchanged result)))
        (is (= before (texts root))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-012
;; @spec MCP-OP-PLAN-013
;; Fence review 2026-10-21 (F2): the refusal's next_call dropped into_existing
;; and the anchor, and for a new namespace it equalled the refused call.
(deftest the-refusals-next-call-carries-the-move-and-runs-as-given
  (let [root (workspace!)]
    (try
      (let [refused (mcp-tool/execute-request! (config root) (move-request))
            next-call (:next_call refused)]
        (is (= "extraction-decisions-required" (:error_type refused)))
        (is (true? (get-in next-call [:extraction :into_existing])))
        (is (= "email" (get-in next-call [:extraction :below])))
        (is (= "accept-proposed" (get-in next-call [:extraction :callers])))
        (is (string? (get-in next-call [:extraction :callers_proposal_hash])))
        (is (not (contains? (:extraction next-call) :caller_changes)))
        (testing "sending it as given completes the move"
          (let [result (mcp-tool/execute-request! (config root) (dissoc next-call :workspace_root))]
            (is (:ok result) (pr-str result))
            (is (str/includes? (slurp (io/file root "src/billing/report.clj")) "customer/has-email?")))))
      (finally (delete-tree! root)))))

(deftest a-refusal-with-unrewritable-callers-leaves-the-decision-open
  (let [root (workspace!)]
    (try
      (spit (io/file root "src/billing/dev.clj")
            "(ns billing.dev)\n\n(defn go [] (require 'billing.invoice) ((resolve 'billing.invoice/has-email?) {}))\n")
      (let [refused (mcp-tool/execute-request! (config root) (move-request))
            extraction (get-in refused [:next_call :extraction])]
        (is (= "extraction-decisions-required" (:error_type refused)))
        (is (= ["src/billing/dev.clj"] (mapv :file (:caller_problems refused))))
        (is (not (contains? extraction :callers)))
        (is (nil? (:caller_changes extraction)))
        (is (contains? extraction :caller_changes)))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-013
;; Fence review 2026-10-21 (F7): acceptance wrote whatever apply discovered,
;; not the proposal that was reviewed, and planning dropped source_roots.
(deftest acceptance-is-bound-to-the-reviewed-proposal
  (let [root (workspace!)]
    (try
      (let [plan (extraction-plan/plan!
                   {:project-root (.getPath root)}
                   {:mode "plan-extraction" :file "src/billing/invoice.clj"
                    :to "src/billing/customer.clj" :forms ["has-email?"]
                    :require_policy "minimal" :into_existing true :below "email"
                    :source_roots ["src"]})
            hash (get-in plan [:plan :callers-proposal-hash])]
        (is (string? hash))
        (is (= ["src"] (get-in plan [:next_call :extraction :source_roots])))
        (spit (io/file root "src/billing/later.clj")
              "(ns billing.later\n  (:require [billing.invoice :as inv]))\n\n(defn f [c] (inv/has-email? c))\n")
        (let [before (texts root)
              result (mcp-tool/execute-request!
                       (config root)
                       (move-request :callers "accept-proposed" :callers_proposal_hash hash))]
          (is (= "stale-caller-proposal" (:error_type result)))
          (is (= before (texts root)))))
      (finally (delete-tree! root)))))

;; @spec MCP-OP-PLAN-014
;; Fence re-review 2026-10-22: planning read the whole workspace without the
;; file and byte bounds, and a missing requested root was dropped silently.
(deftest planning-applies-the-walk-bounds-and-refuses-missing-roots
  (let [root (workspace!)
        plan (fn [config extra]
               (extraction-plan/plan!
                 (merge {:project-root (.getPath root)} config)
                 (merge {:mode "plan-extraction" :file "src/billing/invoice.clj"
                         :to "src/billing/customer.clj" :forms ["has-email?"]
                         :require_policy "minimal" :into_existing true :below "email"}
                        extra)))]
    (try
      (is (= "workspace-scan-budget-exceeded" (:error_type (plan {:max-workspace-files 2} {}))))
      (is (= "invalid-source-roots" (:error_type (plan {} {:source_roots ["nope"]}))))
      (is (= "invalid-source-roots"
             (:error_type (mcp-tool/execute-request! (config root) (move-request :callers "accept-proposed"
                                                                                 :source_roots ["nope"])))))
      (finally (delete-tree! root)))))
