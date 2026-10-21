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
