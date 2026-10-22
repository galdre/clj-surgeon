(ns clj-surgeon.move-callers-test
  "Rewriting callers of forms moved into an existing namespace
  (docs/plans/move-into-existing-namespace.md, \"Callers\"). Pure tests over
  literal sources. Running example: has-email? moves from billing.invoice
  into billing.customer."
  (:require
   [clj-surgeon.move-callers :as callers]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.parser :as parser]))

(defn- rewrite [text & {:as extra}]
  (callers/rewrite-caller
    (merge {:file "src/billing/report.clj"
            :text text
            :source-ns "billing.invoice"
            :target-ns "billing.customer"
            :target-file "src/billing/customer.clj"
            :moved #{"has-email?"}
            :mode :caller}
           extra)))

(defn- parses? [text]
  (some? (parser/parse-string-all text)))

;; ============================================================
;; Aliased references (decision 1)
;; ============================================================

(deftest an-aliased-call-uses-a-new-target-alias-at-its-sorted-position
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]\n"
                        "   [clojure.string :as str]))\n\n"
                        "(defn row [c] [(inv/has-email? c) (inv/total c) (str/trim \"\")])\n"))]
    (is (:ok r))
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.customer :as customer]\n"
                "   [billing.invoice :as inv]\n"
                "   [clojure.string :as str]))\n\n"
                "(defn row [c] [(customer/has-email? c) (inv/total c) (str/trim \"\")])\n")
           (:text r)))
    (is (= ["billing.customer"] (:requires-added r)))
    (is (= [{:line 6 :from "inv/has-email?" :to "customer/has-email?"}] (:rewritten r)))))

(deftest an-alias-only-the-moved-calls-used-is-removed
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn row [c] (inv/has-email? c))\n"))]
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.customer :as customer]))\n\n"
                "(defn row [c] (customer/has-email? c))\n")
           (:text r)))
    (is (= ["billing.invoice"] (:requires-removed r)))))

(deftest an-alias-still-used-by-a-keyword-stays
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn row [c] [(inv/has-email? c) ::inv/id])\n"))]
    (is (str/includes? (:text r) "[billing.invoice :as inv]"))
    (is (= [] (:requires-removed r)))))

(deftest the-callers-own-target-alias-is-used
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.customer :as cust]\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn row [c] [(inv/has-email? c) (cust/email c) (inv/total c)])\n"))]
    (is (str/includes? (:text r) "(cust/has-email? c)"))
    (is (= [] (:requires-added r)))
    (is (= 1 (count (re-seq #"billing\.customer" (:text r)))))))

(deftest a-target-libspec-without-an-alias-gains-one
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.customer :refer [email]]\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn row [c] [(inv/has-email? c) (email c) (inv/total c)])\n"))]
    (is (str/includes? (:text r) "[billing.customer :refer [email] :as customer]"))
    (is (str/includes? (:text r) "(customer/has-email? c)"))))

(deftest a-taken-alias-is-numbered
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]\n"
                        "   [crm.customer :as customer]))\n\n"
                        "(defn row [c] [(inv/has-email? c) (customer/id c) (inv/total c)])\n"))]
    (is (str/includes? (:text r) "[billing.customer :as customer2]"))
    (is (str/includes? (:text r) "(customer2/has-email? c)"))))

;; ============================================================
;; Fully qualified references and var quotes (decision 2)
;; ============================================================

;; Field shape (invoice trial follow-up): tests reach private helpers through a
;; fully qualified var quote.
(deftest a-fully-qualified-var-quote-keeps-its-spelling
  (let [r (rewrite (str "(ns billing.invoice-test\n"
                        "  (:require\n"
                        "   [billing.invoice]\n"
                        "   [clojure.test :refer [deftest is]]))\n\n"
                        "(deftest has-email\n"
                        "  (is (#'billing.invoice/has-email? {:email \"a@b\"})))\n")
                   :file "test/billing/invoice_test.clj")]
    (is (:ok r))
    (is (str/includes? (:text r) "(#'billing.customer/has-email? {:email \"a@b\"})"))
    (testing "a bare target libspec is added; the bare source libspec stays"
      (is (str/includes? (:text r) "   [billing.customer]\n   [billing.invoice]\n")))
    (is (parses? (:text r)))))

(deftest a-fully-qualified-call-with-the-target-already-required-adds-nothing
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.customer :as c]\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn row [x] [(billing.invoice/has-email? x) (c/email x) (inv/total x)])\n"))]
    (is (str/includes? (:text r) "(billing.customer/has-email? x)"))
    (is (= [] (:requires-added r)))))

(deftest a-var-quote-through-an-alias-is-rewritten
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(def checks [#'inv/has-email? (var inv/has-email?)])\n"))]
    (is (str/includes? (:text r) "[#'customer/has-email? (var customer/has-email?)]"))))

;; ============================================================
;; Referred names (decision 3)
;; ============================================================

(deftest a-referred-name-moves-to-the-target-libspec
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :refer [has-email? total]]))\n\n"
                        "(defn row [c] [(has-email? c) (total c)])\n"))]
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.customer :refer [has-email?]]\n"
                "   [billing.invoice :refer [total]]))\n\n"
                "(defn row [c] [(has-email? c) (total c)])\n")
           (:text r)))))

(deftest a-libspec-whose-only-refer-moved-is-removed
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.customer :as c]\n"
                        "   [billing.invoice :refer [has-email?]]))\n\n"
                        "(defn row [x] [(has-email? x) (c/email x)])\n"))]
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.customer :as c :refer [has-email?]]))\n\n"
                "(defn row [x] [(has-email? x) (c/email x)])\n")
           (:text r)))
    (is (= ["billing.invoice"] (:requires-removed r)))))

(deftest an-unused-referred-moved-name-is-dropped
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv :refer [has-email?]]))\n\n"
                        "(defn row [c] (inv/total c))\n"))]
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.invoice :as inv]))\n\n"
                "(defn row [c] (inv/total c))\n")
           (:text r)))))

;; ============================================================
;; Quoted symbols (decision 5), strings (decision 6)
;; ============================================================

(deftest quoted-symbols-are-rewritten
  (testing "fully qualified: exact"
    (let [r (rewrite (str "(ns billing.report)\n\n"
                          "(def check (requiring-resolve 'billing.invoice/has-email?))\n"))]
      (is (str/includes? (:text r) "'billing.customer/has-email?"))
      (is (= [] (:warnings r)))))
  (testing "alias-qualified: rewritten, with a warning"
    (let [r (rewrite (str "(ns billing.report\n"
                          "  (:require [billing.invoice :as inv]))\n\n"
                          "(defn check [] (resolve 'inv/has-email?))\n"))]
      (is (str/includes? (:text r) "(resolve 'customer/has-email?)"))
      (is (= [{:type :quoted-alias-symbol :file "src/billing/report.clj" :line 4
               :from "inv/has-email?" :to "customer/has-email?"}]
             (:warnings r))))))

(deftest syntax-quoted-references-are-rewritten-without-a-warning
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require [billing.invoice :as inv]))\n\n"
                        "(defmacro check [c] `(inv/has-email? ~c))\n"))]
    (is (str/includes? (:text r) "`(customer/has-email? ~c)"))
    (is (= [] (:warnings r)))))

(deftest strings-are-not-examined
  (let [text (str "(ns billing.report)\n\n"
                  "(defn doc [] \"see billing.invoice/has-email?\")\n")
        r (rewrite text)]
    (is (:ok r))
    (is (false? (:changed r)))
    (is (= text (:text r)))))

;; ============================================================
;; Files that are not callers
;; ============================================================

(deftest a-local-with-the-moved-name-is-not-a-caller
  (let [text (str "(ns billing.report\n"
                  "  (:require [billing.invoice :as inv]))\n\n"
                  "(defn row [has-email?] [has-email? (inv/total 1)])\n")
        r (rewrite text)]
    (is (false? (:changed r)))
    (is (= text (:text r)))))

(deftest comment-forms-are-rewritten-like-code
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require [billing.invoice :as inv]))\n\n"
                        "(defn row [c] (inv/total c))\n\n"
                        "(comment (inv/has-email? {}))\n"))]
    (is (str/includes? (:text r) "(comment (customer/has-email? {}))"))))

;; ============================================================
;; The target itself (decision 5 of the plan: plain names)
;; ============================================================

(deftest the-target-calls-moved-forms-by-plain-name
  (let [r (rewrite (str "(ns billing.customer\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv]))\n\n"
                        "(defn email [c] (:email c))\n\n"
                        "(defn reachable? [c] (and (inv/has-email? c) #'inv/has-email?))\n")
                   :file "src/billing/customer.clj"
                   :mode :target)]
    (is (= (str "(ns billing.customer)\n\n"
                "(defn email [c] (:email c))\n\n"
                "(defn reachable? [c] (and (has-email? c) #'has-email?))\n")
           (:text r)))
    (is (= ["billing.invoice"] (:requires-removed r)))))

(deftest the-target-drops-a-refer-of-a-moved-name
  (let [r (rewrite (str "(ns billing.customer\n"
                        "  (:require\n"
                        "   [billing.invoice :as inv :refer [has-email?]]))\n\n"
                        "(defn reachable? [c] [(has-email? c) (inv/total c)])\n")
                   :file "src/billing/customer.clj"
                   :mode :target)]
    (is (str/includes? (:text r) "[billing.invoice :as inv]))"))
    (is (str/includes? (:text r) "[(has-email? c) (inv/total c)]"))))

(deftest a-quoted-qualified-symbol-in-the-target-stays-fully-qualified
  (let [r (rewrite (str "(ns billing.customer)\n\n"
                        "(def check 'billing.invoice/has-email?)\n")
                   :file "src/billing/customer.clj"
                   :mode :target)]
    (is (str/includes? (:text r) "'billing.customer/has-email?"))))

;; One token text, two contexts: the quoted one stays qualified.
(deftest the-same-token-quoted-and-unquoted-in-the-target
  (let [r (rewrite (str "(ns billing.customer)\n\n"
                        "(def both [(billing.invoice/has-email? {}) 'billing.invoice/has-email?])\n")
                   :file "src/billing/customer.clj"
                   :mode :target)]
    (is (str/includes? (:text r) "[(has-email? {}) 'billing.customer/has-email?]"))))

;; ============================================================
;; Refusals
;; ============================================================

(deftest a-platform-that-cannot-load-the-target-refuses
  (let [r (rewrite (str "(ns billing.view\n"
                        "  (:require [billing.invoice :as inv]))\n\n"
                        "(defn row [c] (inv/has-email? c))\n")
                   :file "src/billing/view.cljs")]
    (is (false? (:ok r)))
    (is (= [{:file "src/billing/view.cljs" :line 4 :reason :target-platform
             :text "inv/has-email?"}]
           (:problems r)))))

;; ============================================================
;; Other caller shapes (decision page "Move Into: Caller Shapes")
;; ============================================================

(deftest refer-all-gains-a-refer-of-the-moved-name
  (testing ":refer :all stays; the moved name is referred from the target"
    (let [r (rewrite (str "(ns billing.report\n"
                          "  (:require\n"
                          "   [billing.invoice :refer :all]))\n\n"
                          "(defn row [c] [(has-email? c) (total c)])\n"))]
      (is (= (str "(ns billing.report\n"
                  "  (:require\n"
                  "   [billing.customer :refer [has-email?]]\n"
                  "   [billing.invoice :refer :all]))\n\n"
                  "(defn row [c] [(has-email? c) (total c)])\n")
             (:text r)))))
  (testing ":use stays the same way"
    (let [r (rewrite (str "(ns billing.report\n"
                          "  (:use billing.invoice))\n\n"
                          "(defn row [c] (has-email? c))\n"))]
      (is (= (str "(ns billing.report\n"
                  "  (:use billing.invoice)\n"
                  "  (:require\n"
                  "   [billing.customer :refer [has-email?]]))\n\n"
                  "(defn row [c] (has-email? c))\n")
             (:text r)))))
  (testing ":refer :all without the name is not a caller"
    (is (false? (:changed (rewrite (str "(ns billing.report\n"
                                        "  (:require [billing.invoice :refer :all]))\n\n"
                                        "(defn row [c] (total c))\n")))))))

(deftest a-rename-moves-to-the-target-libspec
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require\n"
                        "   [billing.invoice :refer [has-email? total] :rename {has-email? email?}]))\n\n"
                        "(defn row [c] [(email? c) (total c)])\n"))]
    (is (= (str "(ns billing.report\n"
                "  (:require\n"
                "   [billing.customer :refer [has-email?] :rename {has-email? email?}]\n"
                "   [billing.invoice :refer [total]]))\n\n"
                "(defn row [c] [(email? c) (total c)])\n")
           (:text r))))
  (testing "a rename the file no longer uses is dropped"
    (let [r (rewrite (str "(ns billing.report\n"
                          "  (:require\n"
                          "   [billing.invoice :as inv :refer [has-email?] :rename {has-email? email?}]))\n\n"
                          "(defn row [c] (inv/total c))\n"))]
      (is (str/includes? (:text r) "[billing.invoice :as inv]))"))))
  (testing "in the target, a renamed use would need scope analysis, so it refuses"
    (let [r (rewrite (str "(ns billing.customer\n"
                          "  (:require [billing.invoice :refer [has-email?] :rename {has-email? email?}]))\n\n"
                          "(defn row [c] (email? c))\n")
                     :file "src/billing/customer.clj"
                     :mode :target)]
      (is (= [:rename] (mapv :reason (:problems r)))))))

;; Field case (a large open-source codebase): moving app.util/lower-case-en
;; refused only on references like this one, in .cljc callers.
(deftest a-reference-in-a-reader-conditional-is-rewritten-when-the-target-serves-the-caller
  (let [text (str "(ns billing.report\n"
                  "  (:require [billing.invoice :as inv]))\n\n"
                  "(defn row [c] #?(:clj (inv/has-email? c) :cljs nil))\n")]
    (testing "a .cljc target serves both platforms"
      (let [r (rewrite text :file "src/billing/report.cljc" :target-file "src/billing/customer.cljc")]
        (is (str/includes? (:text r) "#?(:clj (customer/has-email? c) :cljs nil)"))
        (is (str/includes? (:text r) "[billing.customer :as customer]"))))
    (testing "a .clj target cannot serve a .cljc caller"
      (let [r (rewrite text :file "src/billing/report.cljc" :target-file "src/billing/customer.clj")]
        (is (= [:target-platform] (mapv :reason (:problems r))))))))

(deftest a-source-libspec-in-a-reader-conditional-refuses
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require #?(:clj [billing.invoice :as inv])))\n\n"
                        "(defn row [c] (inv/has-email? c))\n")
                   :file "src/billing/report.cljc"
                   :target-file "src/billing/customer.cljc")]
    (is (= [:reader-conditional] (mapv :reason (:problems r))))))

(deftest a-prefix-list-require-hides-the-alias-and-refuses
  (let [r (rewrite (str "(ns billing.report\n"
                        "  (:require (billing [invoice :as inv])))\n\n"
                        "(defn row [c] (inv/has-email? c))\n"))]
    (is (= [:prefix-list] (mapv :reason (:problems r))))))

(deftest a-runtime-require-in-a-comment-form-is-left-with-a-warning
  (let [text (str "(ns billing.report)\n\n"
                  "(comment\n"
                  "  (require '[billing.invoice :as inv])\n"
                  "  (inv/has-email? {}))\n")
        r (rewrite text)]
    (is (:ok r))
    (is (= text (:text r)))
    (is (= [{:type :runtime-require-in-comment :file "src/billing/report.clj" :line 4}]
           (:warnings r)))))

(deftest a-runtime-require-outside-a-comment-form-refuses
  (let [r (rewrite (str "(ns user)\n\n"
                        "(defn load-billing! []\n"
                        "  (require '[billing.invoice :as inv])\n"
                        "  ((resolve 'inv/has-email?) {}))\n")
                   :file "dev/user.clj")]
    (is (= [:runtime-require] (mapv :reason (:problems r))))))

(deftest a-file-with-no-ns-form-that-calls-a-moved-form-refuses
  ;; A script loaded with load-file: there is no ns form to add the require to.
  (let [r (rewrite ";; loaded after billing.invoice\n\n(billing.invoice/has-email? {})\n"
                   :file "dev/scratch.clj")]
    (is (= [{:file "dev/scratch.clj" :line 3 :reason :no-ns-form
             :text "billing.invoice/has-email?"}]
           (:problems r)))))

(deftest an-unparsable-candidate-refuses
  (let [r (rewrite "(ns billing.report)\n\n(defn row [c] (billing.invoice/has-email? c)\n")]
    (is (= [:unparsable] (mapv :reason (:problems r))))))

;; ============================================================
;; Reading only the leading ns form (the require graph reads every file)
;; ============================================================

(deftest the-leading-ns-form-is-read-without-parsing-the-rest
  (is (= "(ns a.b\n  \"doc )\"\n  (:require [c.d :as d]) ; )\n  )"
         (callers/leading-ns-text
           (str ";; header (\n\n(ns a.b\n  \"doc )\"\n  (:require [c.d :as d]) ; )\n  )\n\n"
                "(defn broken [] (\n"))))
  (is (= "(ns a (:require [b \\)]))" (callers/leading-ns-text "(ns a (:require [b \\)]))")))
  (testing "no leading ns form"
    (is (nil? (callers/leading-ns-text "(defn f [])\n(ns a)\n")))
    (is (nil? (callers/leading-ns-text "(ns a (:require [b]")))
    (is (nil? (callers/leading-ns-text "(nsx a)")))))

;; ============================================================
;; Local capture (eval-side report, 2026-10-21: capture through locals on
;; rename and extract)
;; ============================================================

(deftest a-plain-name-a-local-would-capture-refuses-in-the-target
  (testing "a parameter with the moved name"
    (let [r (rewrite (str "(ns billing.customer\n"
                          "  (:require [billing.invoice :as inv]))\n\n"
                          "(defn check [has-email?] (inv/has-email? has-email?))\n")
                     :file "src/billing/customer.clj"
                     :mode :target)]
      (is (= [{:file "src/billing/customer.clj" :line 4 :reason :local-capture
               :text "inv/has-email?"}]
             (:problems r)))))
  (testing ":keys, :strs and namespaced :keys destructuring bind the name too"
    (doseq [binding ["{:keys [has-email?]}" "{:strs [has-email?]}" "{:billing/keys [has-email?]}"
                     "[_ has-email?]" "{has-email? :x}"]]
      (let [r (rewrite (str "(ns billing.customer\n"
                            "  (:require [billing.invoice :as inv]))\n\n"
                            "(defn check [m] (let [" binding " m] (inv/has-email? m)))\n")
                       :file "src/billing/customer.clj"
                       :mode :target)]
        (is (= [:local-capture] (mapv :reason (:problems r))) binding))))
  (testing "a binding form the analysis does not model (a record field) counts as capture"
    (let [r (rewrite (str "(ns billing.customer\n"
                          "  (:require [billing.invoice :as inv]))\n\n"
                          "(defrecord Check [has-email?] Object (toString [_] (str (inv/has-email? {}))))\n")
                     :file "src/billing/customer.clj"
                     :mode :target)]
      (is (= [:local-capture] (mapv :reason (:problems r))))))
  (testing "a local outside the reference's scope does not capture"
    (let [r (rewrite (str "(ns billing.customer\n"
                          "  (:require [billing.invoice :as inv]))\n\n"
                          "(defn check [m] [(let [has-email? 1] has-email?) (inv/has-email? m)])\n")
                     :file "src/billing/customer.clj"
                     :mode :target)]
      (is (:ok r))
      (is (str/includes? (:text r) "(has-email? m)")))))

(deftest capture-probes-report-the-locals-in-scope
  (is (= {"__p1__" #{"email" "c"} "__p2__" #{}}
         (callers/locals-at-probes
           "(defn f [{:keys [email] :as c}] (__p1__ c)) (defn g [] (__p2__))"
           ["__p1__" "__p2__"]))))

;; Eval-side report (2026-10-21): an analysis reported one call twice, and
;; applying both mangled a4/f4 into a4/volume-discountlume-discount.
(deftest a-token-planned-twice-is-replaced-once
  (let [splice #'callers/apply-replacements
        r {:line 1 :col 2 :end-col 7 :from "a4/f4" :to "a4/volume-discount"}]
    (is (= "(a4/volume-discount x)" (splice "(a4/f4 x)" [r r])))
    (testing "overlapping ranges refuse instead of mangling"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"overlap"
            (splice "(a4/f4 x)" [r (assoc r :col 3 :end-col 7 :from "4/f4")]))))))
