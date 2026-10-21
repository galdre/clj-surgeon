(ns clj-surgeon.move-into-shell-test
  "File boundary of moving forms into an existing namespace: hash fences,
  atomic writes, lint rollback, receipts and undo. The planner itself is
  covered by move-into-test with literal sources."
  (:require
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
