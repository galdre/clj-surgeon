(ns clj-surgeon.move-into-shell-test
  "File boundary of moving forms into an existing namespace: hash fences,
  atomic writes, lint rollback, receipts and undo. The planner itself is
  covered by move-into-test with literal sources."
  (:require
   [babashka.fs :as fs]
   [clj-surgeon.move-into-shell :as shell]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- delete-recursive! [f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive! child)))
  (.delete f))

;; Field case (invoice refactoring trial, 2026-10-21): the agent could not move
;; has-email? from billing.invoice into the existing billing.customer.
(defn- invoice-project! []
  (let [root (doto (java.io.File/createTempFile "move-into" "") .delete .mkdirs)
        dir (doto (io/file root "src" "billing") .mkdirs)]
    (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
    (spit (io/file dir "customer.clj")
          (str "(ns billing.customer\n"
               "  (:refer-clojure :exclude [name]))\n\n"
               "(defn name [c] (:name c))\n\n"
               "(defn email [c] (:email c))\n"))
    (spit (io/file dir "invoice.clj")
          (str "(ns billing.invoice\n"
               "  (:require\n"
               "   [billing.customer :as customer]\n"
               "   [clojure.string :as str]))\n\n"
               "(defn- has-email? [customer]\n"
               "  (not (str/blank? (customer/email customer))))\n\n"
               "(defn deliver! [customer]\n"
               "  (when (has-email? customer)\n"
               "    [:queued (customer/email customer) (customer/name customer)]))\n"))
    root))

(defn- opts [root & {:as extra}]
  (merge {:file (str (io/file root "src" "billing" "invoice.clj"))
          :forms '[has-email?]
          :to (str (io/file root "src" "billing" "customer.clj"))
          :into-existing true
          :below "email"
          :lint (fn [_files] {:checked false})}
         extra))

(deftest the-trial-move-succeeds-end-to-end
  (let [root (invoice-project!)
        customer (io/file root "src" "billing" "customer.clj")
        invoice (io/file root "src" "billing" "invoice.clj")]
    (try
      (let [result (shell/execute! (opts root))]
        (is (:applied result))
        (is (= ["has-email?"] (:public-forms result)))
        (is (= ["clojure.string"] (:requires-added result)))
        (is (= (str "(ns billing.customer\n"
                    "  (:refer-clojure :exclude [name])\n"
                    "  (:require\n"
                    "   [clojure.string :as str]))\n\n"
                    "(defn name [c] (:name c))\n\n"
                    "(defn email [c] (:email c))\n\n"
                    "(defn has-email? [customer]\n"
                    "  (not (str/blank? (email customer))))\n")
               (slurp customer)))
        (is (str/includes? (slurp invoice) "[billing.customer :as customer :refer [has-email?]]"))
        (is (not (str/includes? (slurp invoice) "defn- has-email?")))
        (testing "the result stays small and carries no file contents"
          (is (< (count (pr-str result)) 4096))))
      (finally (delete-recursive! root)))))

(deftest a-refusal-leaves-both-files-unchanged
  (let [root (invoice-project!)
        customer (io/file root "src" "billing" "customer.clj")
        invoice (io/file root "src" "billing" "invoice.clj")
        before [(slurp customer) (slurp invoice)]]
    (try
      (let [result (shell/execute! (opts root :below "nope"))]
        (is (= :anchor-not-found (:error-type result)))
        (is (= before [(slurp customer) (slurp invoice)])))
      (finally (delete-recursive! root)))))

(deftest an-existing-target-still-needs-the-flag
  (let [root (invoice-project!)]
    (try
      (is (= :into-existing-target-missing
             (:error-type (shell/execute! (opts root :to (str (io/file root "src" "billing" "nope.clj")))))))
      (finally (delete-recursive! root)))))

(deftest a-new-lint-error-rolls-both-files-back
  (let [root (invoice-project!)
        customer (io/file root "src" "billing" "customer.clj")
        invoice (io/file root "src" "billing" "invoice.clj")
        before [(slurp customer) (slurp invoice)]
        calls (atom 0)
        ;; The first call is the baseline; the second sees one new error.
        lint (fn [_files]
               (if (= 1 (swap! calls inc))
                 {:checked true :findings []}
                 {:checked true
                  :findings [{:filename "src/billing/customer.clj" :type :unresolved-symbol
                              :level :error :message "Unresolved symbol: x"}]}))]
    (try
      (let [result (shell/execute! (opts root :lint lint))]
        (is (= :lint-regression (:error-type result)))
        (is (= 1 (count (:new-errors result))))
        (is (true? (:restored result)))
        (is (= before [(slurp customer) (slurp invoice)])))
      (finally (delete-recursive! root)))))

(deftest a-receipt-undoes-the-move
  (let [root (invoice-project!)
        customer (io/file root "src" "billing" "customer.clj")
        invoice (io/file root "src" "billing" "invoice.clj")
        receipt (io/file root "move.edn")
        before [(slurp customer) (slurp invoice)]]
    (try
      (is (:applied (shell/execute! (opts root :receipt-out (str receipt)))))
      (is (not= before [(slurp customer) (slurp invoice)]))
      (let [undo (shell/undo! {:receipt (str receipt)})]
        (is (:restored undo))
        (is (= before [(slurp customer) (slurp invoice)])))
      (finally (delete-recursive! root)))))

(deftest undo-refuses-when-a-file-changed-after-the-move
  (let [root (invoice-project!)
        customer (io/file root "src" "billing" "customer.clj")
        receipt (io/file root "move.edn")]
    (try
      (shell/execute! (opts root :receipt-out (str receipt)))
      (spit customer "(ns billing.customer)\n")
      (let [undo (shell/undo! {:receipt (str receipt)})]
        (is (= :receipt-result-changed (:error-type undo)))
        (is (= "(ns billing.customer)\n" (slurp customer))))
      (finally (delete-recursive! root)))))

;; Field shape (invoice trial follow-up): the test file reaches the private
;; helper through a fully qualified var quote, so the move must rewrite it.
(defn- invoice-project-with-test! []
  (let [root (invoice-project!)
        dir (doto (io/file root "test" "billing") .mkdirs)]
    (spit (io/file root "deps.edn") "{:paths [\"src\"] :aliases {:test {:extra-paths [\"test\"]}}}\n")
    (spit (io/file dir "invoice_test.clj")
          (str "(ns billing.invoice-test\n"
               "  (:require\n"
               "   [billing.invoice]\n"
               "   [clojure.test :refer [deftest is]]))\n\n"
               "(deftest has-email\n"
               "  (is (#'billing.invoice/has-email? {:email \"a@b\"})))\n"))
    (doto (io/file root "target" "classes" "billing") .mkdirs)
    (spit (io/file root "target" "classes" "billing" "stale_copy.clj")
          "(ns billing.stale-copy\n  (:require [billing.invoice :as inv]))\n\n(defn f [c] (inv/has-email? c))\n")
    root))

(deftest callers-in-other-files-are-rewritten-and-linted-together
  (let [root (invoice-project-with-test!)
        test-file (io/file root "test" "billing" "invoice_test.clj")
        build-copy (io/file root "target" "classes" "billing" "stale_copy.clj")
        build-copy-before (slurp build-copy)
        linted (atom [])
        lint (fn [files] (swap! linted conj (set (map #(.getName (io/file (str %))) files)))
               {:checked true :findings []})]
    (try
      (let [result (shell/execute! (opts root :lint lint))]
        (is (:applied result))
        (is (str/includes? (slurp test-file) "(#'billing.customer/has-email? {:email \"a@b\"})"))
        (is (str/includes? (slurp test-file) "   [billing.customer]\n   [billing.invoice]\n"))
        (testing "files are named relative to the workspace, so the result stays small"
          (is (= [{:file "test/billing/invoice_test.clj" :rewritten 1
                   :requires-added ["billing.customer"] :requires-removed []}]
                 (:callers-rewritten result)))
          (is (= ["test/billing/invoice_test.clj"] (:callers-to-review result))))
        (testing "lint saw every file the move rewrote, before and after"
          (is (= [#{"invoice.clj" "customer.clj" "invoice_test.clj"}
                  #{"invoice.clj" "customer.clj" "invoice_test.clj"}]
                 @linted)))
        (testing "files outside the declared source roots are not callers"
          (is (= build-copy-before (slurp build-copy)))))
      (finally (delete-recursive! root)))))

(deftest a-receipt-undoes-every-rewritten-file
  (let [root (invoice-project-with-test!)
        files (mapv #(io/file root %) ["src/billing/customer.clj" "src/billing/invoice.clj"
                                       "test/billing/invoice_test.clj"])
        receipt (io/file root "move.edn")
        before (mapv slurp files)]
    (try
      (is (:applied (shell/execute! (opts root :receipt-out (str receipt)))))
      (is (every? true? (map not= before (map slurp files))))
      (is (:restored (shell/undo! {:receipt (str receipt)})))
      (is (= before (mapv slurp files)))
      (finally (delete-recursive! root)))))

(deftest a-caller-changed-after-planning-refuses
  (let [root (invoice-project-with-test!)
        test-file (io/file root "test" "billing" "invoice_test.clj")
        customer (io/file root "src" "billing" "customer.clj")
        customer-before (slurp customer)]
    (try
      (let [planned (shell/plan (opts root))
            _ (spit test-file (str (slurp test-file) "\n;; edited\n"))
            result (shell/apply-plan! (opts root) planned)]
        (is (= :stale-source (:error-type result)))
        (is (= ["test/billing/invoice_test.clj"] (:changed result)))
        (is (= customer-before (slurp customer))))
      (finally (delete-recursive! root)))))

(deftest source-roots-override-the-declared-roots
  (let [root (invoice-project-with-test!)
        build-copy (io/file root "target" "classes" "billing" "stale_copy.clj")]
    (try
      (testing "a listed root is scanned even though deps.edn does not declare it"
        (let [r (shell/plan (opts root :source-roots ["src" "test" "target/classes"]))]
          (is (:ok r))
          (is (= ["target/classes/billing/stale_copy.clj" "test/billing/invoice_test.clj"]
                 (:callers-to-review r)))))
      (testing "roots outside the workspace refuse"
        (doseq [bad [["../elsewhere"] ["/etc"]]]
          (let [r (shell/execute! (opts root :source-roots bad))]
            (is (= :invalid-source-roots (:error-type r)))
            (is (:files-unchanged r)))))
      (is (str/includes? (slurp build-copy) "inv/has-email?"))
      (finally (delete-recursive! root)))))

;; Verification gate 3 of the plan: clj-kondo is clean on the field
;; regression's results, through the real linter rather than a stub.
(deftest the-field-move-lints-clean-with-the-real-clj-kondo
  (if-not (fs/which "clj-kondo")
    (println "SKIPPED the-field-move-lints-clean-with-the-real-clj-kondo: missing executable clj-kondo")
    (let [root (invoice-project-with-test!)
          files (mapv #(io/file root %) ["src/billing/customer.clj" "src/billing/invoice.clj"
                                         "test/billing/invoice_test.clj"])]
      (try
        (let [result (shell/execute! (dissoc (opts root) :lint))]
          (is (:applied result))
          (is (= {:checked true :files 3 :new-errors 0} (:lint result)))
          (is (= [] (->> (:findings (shell/clj-kondo-lint files))
                         (filter #(= :error (keyword (:level %))))
                         (mapv #(select-keys % [:filename :row :message]))))))
        (finally (delete-recursive! root))))))

;; Eval-side report (2026-10-21): clj-kondo reports a dangling qualified
;; reference (ns/missing) as a warning, so counting only :error findings let a
;; move that leaves one through.
(deftest unresolved-references-block-at-any-level
  (let [finding (fn [type level] {:filename "src/a.clj" :row 3 :type type :level level
                                  :message (str type)})]
    (is (= [:unresolved-var :unresolved-namespace :unresolved-symbol :syntax]
           (mapv :type (shell/new-blocking-findings
                         [(finding :unused-binding :warning)]
                         [(finding :unused-binding :warning)
                          (finding :unresolved-var :warning)
                          (finding :unresolved-namespace :warning)
                          (finding :unresolved-symbol :error)
                          (finding :syntax :error)
                          (finding :redundant-do :warning)]))))
    (testing "a finding the baseline already had does not block"
      (is (= [] (shell/new-blocking-findings [(finding :unresolved-var :warning)]
                                             [(finding :unresolved-var :warning)]))))))

(deftest clj-kondo-reports-a-dangling-qualified-reference
  (if-not (fs/which "clj-kondo")
    (println "SKIPPED clj-kondo-reports-a-dangling-qualified-reference: missing executable clj-kondo")
    (let [root (doto (java.io.File/createTempFile "move-into-kondo" "") .delete .mkdirs)
          a (io/file root "app" "a.clj")
          b (io/file root "app" "b.clj")]
      (try
        (.mkdirs (.getParentFile a))
        (spit a "(ns app.a)\n\n(defn present [] 1)\n")
        (spit b "(ns app.b\n  (:require [app.a :as a]))\n\n(defn f [] [(a/present) (a/moved-away)])\n")
        (is (= [{:type :unresolved-var :message "Unresolved var: a/moved-away"}]
               (mapv #(select-keys % [:type :message])
                     (shell/new-blocking-findings [] (:findings (shell/clj-kondo-lint [a b]))))))
        (finally (delete-recursive! root))))))
