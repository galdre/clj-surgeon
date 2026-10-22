(ns clj-surgeon.move-into-test
  "Moving forms into an existing namespace: docs/plans/move-into-existing-namespace.md.
  Pure tests over literal sources."
  (:require
   [clj-surgeon.move-into :as move-into]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.parser :as parser]))

;; ============================================================
;; Anchors and placement
;; ============================================================

(def ^:private target-with-forms
  (str "(ns app.target)\n\n"
       "(defn alpha [] :a)\n\n"
       ";; beta's comment\n"
       "(defn beta [] :b)\n\n"
       "(defwhatever foo {:a 1})\n\n"
       "(defmulti render :kind)\n\n"
       "(defmethod render :card [x] x)\n\n"
       "(defmethod render :list [x] x)\n\n"
       "(comment (alpha))\n"))

(def ^:private block "(defn moved [] :m)")

(deftest anchor-is-required-and-exclusive
  (is (= :anchor-required
         (:error-type (move-into/resolve-anchor target-with-forms {} {}))))
  (is (= :anchor-required
         (:error-type (move-into/resolve-anchor
                        target-with-forms {:above "alpha" :at-end true} {})))))

(deftest anchors-place-the-block
  (testing "--below puts the block after the named form, one blank line each side"
    (let [r (move-into/place-block target-with-forms {:below "alpha"} {} block)]
      (is (str/includes? (:target r) "(defn alpha [] :a)\n\n(defn moved [] :m)\n\n;; beta's comment\n"))))
  (testing "--above puts the block before the named form's comment"
    (let [r (move-into/place-block target-with-forms {:above "beta"} {} block)]
      (is (str/includes? (:target r) "(defn alpha [] :a)\n\n(defn moved [] :m)\n\n;; beta's comment\n(defn beta"))))
  (testing "--at-start puts the block directly after the ns form"
    (let [r (move-into/place-block target-with-forms {:at-start true} {} block)]
      (is (str/starts-with? (:target r) "(ns app.target)\n\n(defn moved [] :m)\n\n(defn alpha"))))
  (testing "--at-end puts the block after the last form"
    (let [r (move-into/place-block target-with-forms {:at-end true} {} block)]
      (is (str/ends-with? (:target r) "(comment (alpha))\n\n(defn moved [] :m)\n"))))
  (testing "--at-end works on a target holding only its ns form"
    (let [r (move-into/place-block "(ns app.empty)\n" {:at-end true} {} block)]
      (is (= "(ns app.empty)\n\n(defn moved [] :m)\n" (:target r)))))
  (testing "--below the last form, with no trailing newline"
    (let [r (move-into/place-block "(ns a)\n\n(defn x [] 1)" {:below "x"} {} block)]
      (is (= "(ns a)\n\n(defn x [] 1)\n\n(defn moved [] :m)\n" (:target r)))))
  (testing "every placement parses"
    (doseq [anchor [{:below "alpha"} {:above "beta"} {:at-start true} {:at-end true}]]
      (is (some? (parser/parse-string-all
                   (:target (move-into/place-block target-with-forms anchor {} block))))))))

(deftest unknown-and-unrecognized-anchors-refuse
  (testing "a name the target does not define"
    (let [r (move-into/resolve-anchor target-with-forms {:below "nope"} {})]
      (is (= :anchor-not-found (:error-type r)))))
  (testing "an unrecognized defining macro refuses and shows the config entry"
    (let [r (move-into/resolve-anchor target-with-forms {:below "foo"} {})]
      (is (= :anchor-not-found (:error-type r)))
      (is (= "defwhatever" (:unrecognized-macro r)))
      (is (= {:aliases {"defwhatever" {:kind :def}}} (:config-entry r)))
      (is (= ["--at-start" "--at-end"] (:alternatives r)))))
  (testing "the same macro declared in .clj-surgeon.edn resolves"
    (let [r (move-into/resolve-anchor target-with-forms {:below "foo"}
                                      {"defwhatever" {:kind :def}})]
      (is (nil? (:error-type r)))
      (is (= "foo" (:name r)))))
  (testing "a form with no usable name cannot be an anchor"
    (is (= :anchor-not-found
           (:error-type (move-into/resolve-anchor target-with-forms {:below "(alpha)"} {}))))))

(deftest ambiguous-anchors-refuse-with-typed-anchors
  (let [r (move-into/resolve-anchor target-with-forms {:below "render"} {})]
    (is (= :ambiguous-anchor (:error-type r)))
    (is (= ["defmulti render" "defmethod render :card" "defmethod render :list"]
           (mapv :anchor (:matches r)))))
  (testing "a typed anchor names one form exactly"
    (let [r (move-into/place-block target-with-forms
                                   {:below "defmethod render :card"} {} block)]
      (is (str/includes? (:target r)
                         "(defmethod render :card [x] x)\n\n(defn moved [] :m)\n\n(defmethod render :list")))))

;; ============================================================
;; Requires and qualifier rewrites
;; ============================================================

(defn- merge-requires [source-ns target forms]
  (move-into/merge-requires {:source-ns-form source-ns
                             :target target
                             :target-ns "app.target"
                             :form-sources forms}))

(deftest a-library-the-target-already-requires-adds-nothing
  (let [r (merge-requires "(ns app.source (:require [clojure.string :as str]))"
                          "(ns app.target (:require [clojure.string :as str]))\n"
                          ["(defn f [s] (str/blank? s))"])]
    (is (:ok r))
    (is (= [] (:requires-added r)))
    (is (= ["(defn f [s] (str/blank? s))"] (:form-sources r)))))

(deftest a-missing-library-is-inserted-at-its-sorted-position
  (let [r (merge-requires "(ns app.source (:require [b.lib :as b]))"
                          (str "(ns app.target\n"
                               "  (:require\n"
                               "   [a.lib :as a]\n"
                               "   [c.lib :as c]))\n")
                          ["(defn f [] (b/g))"])]
    (is (= ["b.lib"] (:requires-added r)))
    (is (str/includes? (:target r) "[a.lib :as a]\n   [b.lib :as b]\n   [c.lib :as c]"))))

(deftest a-target-with-no-require-clause-gets-one
  (let [r (merge-requires "(ns app.source (:require [b.lib :as b]))"
                          "(ns app.target)\n"
                          ["(defn f [] (b/g))"])]
    (is (str/includes? (:target r) "(:require\n   [b.lib :as b])"))))

(deftest the-same-library-under-another-alias-rewrites-the-moved-qualifiers
  (let [r (merge-requires "(ns app.source (:require [clojure.string :as str]))"
                          "(ns app.target (:require [clojure.string :as string]))\n"
                          ["(defn f [s] (str/blank? s))"])]
    (is (= [] (:requires-added r)))
    (is (= ["(defn f [s] (string/blank? s))"] (:form-sources r)))
    (is (= [{:from "str/blank?" :to "string/blank?"}] (:qualifiers-rewritten r)))))

(deftest the-same-alias-for-another-library-refuses
  (let [r (merge-requires "(ns app.source (:require [clojure.string :as str]))"
                          "(ns app.target (:require [my.strings :as str]))\n"
                          ["(defn f [s] (str/blank? s))"])]
    (is (= :alias-conflict (:error-type r)))
    (is (= [{:alias "str" :source-namespace "clojure.string"
             :target-namespace "my.strings"}]
           (:conflicts r)))))

(deftest calls-into-the-target-become-plain-names
  (let [r (merge-requires "(ns app.source (:require [app.target :as customer]))"
                          "(ns app.target)\n"
                          [(str "(defn has-email? [c]\n"
                                "  (and (::customer/active c) (customer/email c)))")])]
    (is (= [] (:requires-added r)))
    (is (= ["(defn has-email? [c]\n  (and (::active c) (email c)))"] (:form-sources r)))
    (is (not (str/includes? (:target r) "app.target :as")))))

(deftest a-referred-symbol-is-carried-into-a-new-libspec
  (let [r (merge-requires "(ns app.source (:require [b.lib :refer [g h]]))"
                          "(ns app.target)\n"
                          ["(defn f [] (g))"])]
    (is (= ["b.lib"] (:requires-added r)))
    (is (str/includes? (:target r) "[b.lib :refer [g]]"))))

(deftest a-referred-symbol-the-target-does-not-refer-refuses
  (let [r (merge-requires "(ns app.source (:require [b.lib :refer [g]]))"
                          "(ns app.target (:require [b.lib :as b]))\n"
                          ["(defn f [] (g))"])]
    (is (= :refer-merge-unsupported (:error-type r)))
    (is (= [{:namespace "b.lib" :symbols ["g"]}] (:missing-refers r)))))

;; ============================================================
;; The complete move: refusals, warnings, both files
;; ============================================================

(defn- move [source target forms anchor & {:as extra}]
  (move-into/compile-move
    (merge {:source-file "src/app/source.clj"
            :source source
            :target-file "src/app/target.clj"
            :target target
            :target-ns "app.target"
            :forms forms
            :anchor anchor
            :project-aliases {}
            :workspace-sources {}}
           extra)))

(deftest a-name-the-target-defines-refuses
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n\n(defn main [] (f))\n"
                "(ns app.target)\n\n(defn f [] 2)\n"
                '[f] {:at-end true})]
    (is (= :name-clash (:error-type r)))
    (is (= ["f"] (:names r)))))

(deftest a-form-left-behind-refuses-with-the-set-to-move
  (let [r (move (str "(ns app.source)\n\n"
                     "(defn- helper [x] (* 2 x))\n\n"
                     "(defn uses-helper [x] (helper x))\n\n"
                     "(defn main [] (uses-helper 3))\n")
                "(ns app.target)\n"
                '[uses-helper] {:at-end true})]
    (is (= :left-behind-dependencies (:error-type r)))
    (is (= [{:form "uses-helper" :calls ["helper"]}] (:left-behind r)))
    (is (= ["helper" "uses-helper"] (:suggested-forms r)))))

;; Field case (invoice trial): billing.customer excludes clojure.core/name and
;; defines its own name accessor.
(deftest a-symbol-that-means-something-else-in-the-target-refuses
  (let [r (move "(ns app.source)\n\n(defn label [k] (name k))\n\n(defn main [] (label :x))\n"
                (str "(ns app.target\n  (:refer-clojure :exclude [name]))\n\n"
                     "(defn name [c] (:name c))\n")
                '[label] {:at-end true})]
    (is (= :meaning-change (:error-type r)))
    (is (= [{:symbol "name" :in-source "clojure.core/name" :in-target "app.target/name"}]
           (:changes r)))))

(deftest a-symbol-the-target-refers-from-another-library-refuses
  (let [r (move "(ns app.source)\n\n(defn f [xs] (join xs))\n\n(defn main [] (f []))\n"
                "(ns app.target\n  (:require [clojure.string :refer [join]]))\n"
                '[f] {:at-end true})]
    (is (= :meaning-change (:error-type r)))
    (is (= "clojure.string/join" (-> r :changes first :in-target)))))

(deftest a-target-that-calls-the-moved-form-through-the-source-calls-it-by-name
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n"
                "(ns app.target\n  (:require [app.source :as src]))\n\n(defn g [] (src/f))\n"
                '[f] {:at-end true})]
    (is (:ok r))
    (testing "the require of the source goes with its last use, so no cycle"
      (is (= "(ns app.target)\n\n(defn g [] (f))\n\n(defn f [] 1)\n" (:target r))))
    (is (= "(ns app.source)\n" (:source r)))))

;; Field shape (invoice trial follow-up): a test reaches the moved private
;; helper through a fully qualified var quote.
(deftest callers-in-other-files-are-rewritten
  (let [test-file "test/app/source_test.clj"
        report "src/app/report.clj"
        r (move "(ns app.source)\n\n(defn- f [] 1)\n\n(defn main [] (f))\n"
                "(ns app.target)\n"
                '[f] {:at-end true}
                :workspace-sources
                {test-file (str "(ns app.source-test\n"
                                "  (:require\n"
                                "   [app.source]\n"
                                "   [clojure.test :refer [deftest is]]))\n\n"
                                "(deftest f-is-one (is (= 1 (#'app.source/f))))\n")
                 report (str "(ns app.report\n"
                             "  (:require [app.source :as s]))\n\n"
                             "(defn row [] [(s/f) (s/main)])\n")
                 "src/app/unrelated.clj" "(ns app.unrelated)\n\n(defn g [f] (f))\n"})]
    (is (:ok r))
    (is (= [report test-file] (sort (keys (:callers r)))))
    (is (str/includes? (get-in r [:callers test-file]) "(#'app.target/f)"))
    (is (str/includes? (get-in r [:callers test-file]) "   [app.source]\n   [app.target]\n"))
    (is (str/includes? (get-in r [:callers report]) "[(target/f) (s/main)]"))
    (is (= [{:file report :rewritten 1 :requires-added ["app.target"] :requires-removed []}
            {:file test-file :rewritten 1 :requires-added ["app.target"] :requires-removed []}]
           (:callers-rewritten r)))
    (is (= [report test-file] (:callers-to-review r)))))

(deftest a-caller-that-cannot-be-rewritten-refuses-before-any-write
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n"
                "(ns app.target)\n"
                '[f] {:at-end true}
                :workspace-sources
                {"src/app/a.clj" "(ns app.a)\n\n(defn g []\n  (require 'app.source)\n  ((resolve 'app.source/f)))\n"
                 "src/app/b.cljs" "(ns app.b\n  (:require [app.source :as s]))\n\n(defn g [] (s/f))\n"})]
    (is (= :caller-not-rewritable (:error-type r)))
    (is (= [{:file "src/app/a.clj" :line 4 :reason :runtime-require :text "app.source"}
            {:file "src/app/b.cljs" :line 4 :reason :target-platform :text "s/f"}]
           (:references r)))
    (is (:files-unchanged r))))

(deftest a-caller-rewrite-that-would-close-a-require-cycle-refuses
  ;; The target requires app.report; rewriting app.report's call makes
  ;; app.report require the target.
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n"
                "(ns app.target\n  (:require [app.report :as report]))\n\n(defn g [] (report/row))\n"
                '[f] {:at-end true}
                :workspace-sources
                {"src/app/report.clj" "(ns app.report\n  (:require [app.source :as s]))\n\n(defn row [] (s/f))\n"})]
    (is (= :circular-require (:error-type r)))
    (is (= ["app.target" "app.report" "app.target"] (:cycle r)))))

(deftest a-move-that-would-make-namespaces-require-each-other-refuses
  ;; Before the move: target -> middle -> source, no cycle. The source keeps a
  ;; caller of f, so it must require the target, which closes the loop.
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n\n(defn main [] (f))\n"
                "(ns app.target\n  (:require [app.middle :as m]))\n\n(defn g [] (m/h))\n"
                '[f] {:at-end true}
                :workspace-sources {"src/app/middle.clj"
                                    "(ns app.middle\n  (:require [app.source :as s]))\n\n(defn h [] 1)\n"})]
    (is (= :circular-require (:error-type r)))
    (is (= ["app.source" "app.target" "app.middle" "app.source"] (:cycle r)))))

(deftest unqualified-auto-resolved-keywords-move-with-a-warning
  (let [r (move "(ns app.source)\n\n(defn f [m] (::id m))\n\n(defn main [] (f {}))\n"
                "(ns app.target)\n"
                '[f] {:at-end true})]
    (is (:ok r))
    (is (str/includes? (:target r) "(::id m)"))
    (is (= [{:type :auto-resolved-keywords-moved :keywords ["::id"]
             :meaning "These now name the target namespace instead of app.source"}]
           (:warnings r)))))

(deftest a-form-placed-above-what-it-calls-moves-with-a-warning
  (let [r (move "(ns app.source\n  (:require [app.target :as t]))\n\n(defn f [] (t/g))\n\n(defn main [] (f))\n"
                "(ns app.target)\n\n(defn g [] 1)\n"
                '[f] {:above "g"})]
    (is (:ok r))
    (is (str/includes? (:target r) "(defn f [] (g))\n\n(defn g [] 1)"))
    (is (= [{:type :used-before-definition :form "f" :uses ["g"]}]
           (:warnings r)))))

(deftest moved-forms-that-call-each-other-carry-a-declare
  (let [r (move (str "(ns app.source)\n\n(declare odd?*)\n\n"
                     "(defn even?* [n] (if (zero? n) true (odd?* (dec n))))\n\n"
                     "(defn odd?* [n] (if (zero? n) false (even?* (dec n))))\n\n"
                     "(defn main [] (even?* 4))\n")
                "(ns app.target)\n"
                '[even?* odd?*] {:at-end true})]
    (is (:ok r))
    (is (str/includes? (:target r) "(declare odd?*)\n\n(defn even?*"))
    (is (not (str/includes? (:source r) "declare")))))

(deftest the-source-requires-the-target-for-its-remaining-callers
  (testing "a new libspec when the source does not require the target yet"
    (let [r (move "(ns app.source)\n\n(defn f [] 1)\n\n(defn main [] (f))\n"
                  "(ns app.target)\n" '[f] {:at-end true})]
      (is (str/includes? (:source r) "[app.target :as target :refer [f]]"))))
  (testing "a :refer added to the existing libspec when it does"
    (let [r (move (str "(ns app.source\n  (:require [app.target :as t]))\n\n"
                       "(defn f [] (t/g))\n\n(defn main [] [(f) (t/g)])\n")
                  "(ns app.target)\n\n(defn g [] 1)\n" '[f] {:at-end true})]
      (is (str/includes? (:source r) "[app.target :as t :refer [f]]"))
      (is (= 1 (count (re-seq #"app\.target" (:source r)))))))
  (testing "both results parse"
    (let [r (move "(ns app.source)\n\n(defn f [] 1)\n\n(defn main [] (f))\n"
                  "(ns app.target)\n" '[f] {:at-end true})]
      (is (some? (parser/parse-string-all (:source r))))
      (is (some? (parser/parse-string-all (:target r)))))))

(deftest a-private-form-the-source-still-calls-becomes-public
  (let [r (move "(ns app.source)\n\n(defn- f [] 1)\n\n(defn main [] (f))\n"
                "(ns app.target)\n" '[f] {:at-end true})]
    (is (= ["f"] (:public-forms r)))
    (is (str/includes? (:target r) "(defn f [] 1)"))))

;; Field case (invoice trial): after has-email? moved to billing.customer,
;; clojure.string was unused in billing.invoice and clj-kondo warned.
(deftest a-require-only-the-moved-forms-used-leaves-the-source
  (let [r (move (str "(ns app.source\n"
                     "  (:require\n"
                     "   [clojure.set :as set]\n"
                     "   [clojure.string :as str]\n"
                     "   [side.effects]))\n\n"
                     "(defn f [s] (str/blank? s))\n\n"
                     "(defn main [] [(f \"\") (set/union)])\n")
                "(ns app.target)\n" '[f] {:at-end true})]
    (is (= ["clojure.string"] (:requires-removed r)))
    (is (not (str/includes? (:source r) "clojure.string")))
    (testing "a require still used, and a bare side-effect require, stay"
      (is (str/includes? (:source r) "[clojure.set :as set]"))
      (is (str/includes? (:source r) "[side.effects]")))
    (is (some? (parser/parse-string-all (:source r))))))

(deftest a-require-clause-emptied-by-the-move-is-removed
  (let [r (move (str "(ns app.source\n"
                     "  (:require\n"
                     "   [clojure.string :as str]))\n\n"
                     "(defn f [s] (str/blank? s))\n")
                "(ns app.target)\n" '[f] {:at-end true})]
    (is (= "(ns app.source)\n" (:source r)))))

(deftest several-unused-requires-are-all-removed
  (let [r (move (str "(ns app.source\n"
                     "  (:require\n"
                     "   [a.lib :as a]\n"
                     "   [b.lib :as b]\n"
                     "   [c.lib :as c]\n"
                     "   [d.lib :as d]))\n\n"
                     "(defn f [] [(b/x) (d/y)])\n\n"
                     "(defn main [] [(f) (a/z) (c/w)])\n")
                "(ns app.target)\n" '[f] {:at-end true})]
    (is (= ["b.lib" "d.lib"] (:requires-removed r)))
    (is (str/includes? (:source r) (str "   [a.lib :as a]\n"
                                        "   [app.target :as target :refer [f]]\n"
                                        "   [c.lib :as c]))")))
    (is (not (re-find #"[bd]\.lib" (:source r))))))

;; Eval-side report (2026-10-21): customer/email became the plain email inside
;; a {:keys [email]} destructuring, so the moved form called the local string.
(deftest a-qualifier-rewrite-a-local-would-capture-refuses
  (let [r (move (str "(ns app.source\n  (:require [app.target :as t]))\n\n"
                     "(defn f [{:keys [g] :as m}] (and g (t/g m)))\n")
                "(ns app.target)\n\n(defn g [m] (:g m))\n"
                '[f] {:at-end true})]
    (is (= :meaning-change (:error-type r)))
    (is (= [{:symbol "g" :from "t/g" :form "f"}] (:captures r)))))

(deftest a-qualifier-rewrite-outside-any-local-still-becomes-plain
  (let [r (move (str "(ns app.source\n  (:require [app.target :as t]))\n\n"
                     "(defn f [m] (let [x 1] (t/g m x)))\n")
                "(ns app.target)\n\n(defn g [m x] [m x])\n"
                '[f] {:at-end true})]
    (is (:ok r))
    (is (str/includes? (:target r) "(let [x 1] (g m x))"))))

(deftest a-quoted-call-into-the-target-stays-fully-qualified
  (let [r (move (str "(ns app.source\n  (:require [app.target :as t]))\n\n"
                     "(defn f [] [(t/g) (requiring-resolve 't/g)])\n")
                "(ns app.target)\n\n(defn g [] 1)\n"
                '[f] {:at-end true})]
    (is (:ok r))
    (is (str/includes? (:target r) "[(g) (requiring-resolve 'app.target/g)]"))))

;; ============================================================
;; Forms that define more than their name (eval-side report, 2026-10-21:
;; protocol methods)
;; ============================================================

(def ^:private protocol-source
  (str "(ns app.source)\n\n"
       "(defprotocol Shape\n  (area [this])\n  (perimeter [this]))\n\n"
       "(defn main [x] (area x))\n"))

(deftest moving-a-protocol-moves-its-methods
  (let [r (move protocol-source "(ns app.target)\n" '[Shape] {:at-end true}
                :workspace-sources
                {"src/app/use.clj" (str "(ns app.use\n  (:require [app.source :as s]))\n\n"
                                        "(defn f [x] [(s/area x) (s/main x)])\n")})]
    (is (:ok r))
    (testing "the source refers the methods its remaining forms call"
      (is (str/includes? (:source r) "[app.target :as target :refer [area]]")))
    (testing "callers of a method are rewritten"
      (is (str/includes? (get-in r [:callers "src/app/use.clj"]) "[(target/area x) (s/main x)]")))))

(deftest moving-a-record-moves-its-constructors
  (let [source (str "(ns app.source)\n\n(defrecord Circle [r])\n\n(defn main [] (->Circle 1))\n")
        r (move source "(ns app.target)\n" '[Circle] {:at-end true}
                :workspace-sources
                {"src/app/use.clj" (str "(ns app.use\n  (:require [app.source :as s]))\n\n"
                                        "(defn f [] [(s/->Circle 1) (s/map->Circle {:r 1})])\n")})]
    (is (:ok r))
    (is (str/includes? (:source r) ":refer [->Circle]"))
    (is (str/includes? (get-in r [:callers "src/app/use.clj"])
                       "[(target/->Circle 1) (target/map->Circle {:r 1})]"))))

(deftest a-caller-importing-a-moved-record-class-refuses
  (let [r (move "(ns app.source)\n\n(defrecord Circle [r])\n" "(ns app.target)\n" '[Circle] {:at-end true}
                :workspace-sources
                {"src/app/use.clj" (str "(ns app.use\n  (:import [app.source Circle]))\n\n"
                                        "(defn f [^Circle c] (.r c))\n")})]
    (is (= :caller-not-rewritable (:error-type r)))
    (is (= [{:file "src/app/use.clj" :line 2 :reason :class-import :text "Circle"}]
           (:references r)))))

(deftest a-moved-form-calling-a-method-of-a-protocol-left-behind-refuses
  (let [r (move protocol-source "(ns app.target)\n" '[main] {:at-end true})]
    (is (= :left-behind-dependencies (:error-type r)))
    (is (= [{:form "main" :calls ["area"]}] (:left-behind r)))
    (is (= ["Shape" "main"] (:suggested-forms r)))))

(deftest a-moved-name-that-a-target-protocol-defines-clashes
  (let [r (move "(ns app.source)\n\n(defn area [x] x)\n"
                "(ns app.target)\n\n(defprotocol Shape (area [this]))\n"
                '[area] {:at-end true})]
    (is (= :name-clash (:error-type r)))
    (is (= ["area"] (:names r)))))

;; Eval-side report (2026-10-21): shadowing clojure.core.
(deftest a-moved-name-that-shadows-clojure-core-in-the-target-refuses
  (let [source "(ns app.source\n  (:refer-clojure :exclude [update]))\n\n(defn update [m] (assoc m :updated true))\n"]
    (testing "the target still refers clojure.core/update"
      (let [r (move source "(ns app.target)\n\n(defn bump [m] (update m :n inc))\n"
                    '[update] {:at-end true})]
        (is (= :name-clash (:error-type r)))
        (is (= {"update" "clojure.core/update"} (:core-names r)))))
    (testing "a target that already excludes it accepts the move"
      (let [r (move source "(ns app.target\n  (:refer-clojure :exclude [update]))\n"
                    '[update] {:at-end true})]
        (is (:ok r))))))

;; ============================================================
;; Caller authority and the scan bound (MCP route)
;; ============================================================

(def ^:private runtime-caller
  {"src/app/a.clj" "(ns app.a)\n\n(defn g []\n  (require 'app.source)\n  ((resolve 'app.source/f)))\n"
   "src/app/b.clj" "(ns app.b\n  (:require [app.source :as s]))\n\n(defn h [] (s/f))\n"})

(deftest a-model-deciding-its-callers-gets-the-proposal-as-evidence
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n" "(ns app.target)\n" '[f] {:at-end true}
                :workspace-sources runtime-caller
                :caller-authority :model)]
    (is (:ok r))
    (is (= ["src/app/b.clj"] (keys (:callers r))))
    (is (= [:runtime-require] (mapv :reason (:caller-problems r))))
    (testing "the kernel's authority refuses the same request"
      (is (= :caller-not-rewritable
             (:error-type (move "(ns app.source)\n\n(defn f [] 1)\n" "(ns app.target)\n" '[f]
                                {:at-end true} :workspace-sources runtime-caller)))))))

(deftest a-caller-scan-over-its-bound-refuses-before-parsing
  (let [r (move "(ns app.source)\n\n(defn f [] 1)\n" "(ns app.target)\n" '[f] {:at-end true}
                :workspace-sources (assoc runtime-caller "src/app/c.clj" "(ns app.c (:require [app.source]))\n(app.source/f\n")
                :max-candidates 2)]
    (is (= :caller-scan-budget-exceeded (:error-type r)))
    (is (= 3 (:candidates r)))))
