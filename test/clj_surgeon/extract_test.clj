(ns clj-surgeon.extract-test
  (:require
   [clj-surgeon.extract :as extract]
   [clj-surgeon.file-ops :as file-ops]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.parser :as parser]))

(defn- create-temp-project!
  "Create a minimal project with a monolith file. distill does not call
  helper: extracting [distill refine] then leaves a valid program, which the
  extraction's clj-kondo gate requires. (It once called helper, and every
  extraction of these tests wrote a target with an unresolved symbol.)"
  []
  (let [root (java.io.File/createTempFile "extract-test" "")
        _ (.delete root)
        _ (.mkdirs root)
        src-dir (io/file root "src" "my")]
    (.mkdirs src-dir)
    (spit (io/file src-dir "app.clj")
          "(ns my.app
  (:require [clojure.string :as str]))

(def config {:port 3000})

(defn helper [x]
  (str/upper-case x))

;; Distillery functions
(defn distill [x]
  (keyword x))

(defn refine [x]
  (distill (str x \"-refined\")))

;; Main entry
(defn -main []
  (refine \"hello\"))
")
    root))

(defn- delete-recursive! [f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)]
      (delete-recursive! child)))
  (.delete f))

(defn- dependency-minimal-source []
  (str "(ns fixture.views\n  (:require\n"
       (apply str
              (for [index (range 1 17)]
                (format "   [fixture.dep%02d :as d%02d]\n" index index)))
       "   [fixture.domain.schedule :as schedule]))\n\n"
       "(defn day-tab []\n"
       "  [d01/value d02/value d03/value d04/value d05/value d06/value])\n\n"
       "(defn agenda-page []\n  (day-tab))\n\n"
       "(defn unrelated-page [] :ok)\n"))

(defn- create-dependency-minimal-project!
  "Create the minimized production shape from clj-surgeon-to4."
  []
  (let [root (java.io.File/createTempFile "extract-dependencies" "")
        _ (.delete root)
        _ (.mkdirs root)
        src-dir (io/file root "src")
        views-file (io/file src-dir "fixture" "views.clj")]
    (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
    (doseq [index (range 1 17)]
      (let [ns-name (format "fixture.dep%02d" index)
            file (io/file src-dir "fixture" (format "dep%02d.clj" index))]
        (.mkdirs (.getParentFile file))
        (spit file (format "(ns %s)\n\n(def value :dep-%02d)\n" ns-name index))))
    (let [schedule-file (io/file src-dir "fixture" "domain" "schedule.clj")]
      (.mkdirs (.getParentFile schedule-file))
      (spit schedule-file
            "(ns fixture.domain.schedule)\n\n(def existing :schedule)\n"))
    (.mkdirs (.getParentFile views-file))
    (spit views-file (dependency-minimal-source))
    root))

(defn- cold-require-result [root & namespaces]
  (shell/sh "clojure"
            "-Sdeps" (pr-str {:paths [(str (io/file root "src"))]})
            "-M" "-e"
            (str "(require " (str/join " " (map #(str "'" %) namespaces)) ")")))

(defn- cold-eval-result [root expression]
  (shell/sh "clojure"
            "-Sdeps" (pr-str {:paths [(str (io/file root "src"))]})
            "-M" "-e" expression))

;; ============================================================
;; Pure unit tests for file-path->ns-name
;; ============================================================

(deftest test-file-path->ns-name-standard-layout
  (testing "standard src/ layout"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/myapp/core.clj"))))
  (testing "nested namespaces"
    (is (= "myapp.state.distillery"
           (#'extract/file-path->ns-name "src/myapp/state/distillery.clj"))))
  (testing "underscores become hyphens"
    (is (= "my-app.my-ns"
           (#'extract/file-path->ns-name "src/my_app/my_ns.clj"))))
  (testing "absolute path with /src/"
    (is (= "my.app"
           (#'extract/file-path->ns-name "/tmp/project/src/my/app.clj"))))
  (testing "relative path starting with src/"
    (is (= "my.app"
           (#'extract/file-path->ns-name "src/my/app.clj")))))

(deftest test-source-paths-from-deps-edn
  (testing "reads this project's deps.edn correctly"
    (let [paths (#'extract/source-paths-from-deps-edn)]
      (is (sequential? paths))
      (is (some #{"src"} paths)))))

(deftest test-file-path->ns-name-no-deps-edn
  (testing "falls back to [\"src\"] when no deps.edn and no explicit paths"
    ;; Pass empty vector to simulate no source paths found;
    ;; the function should fall through to the /src/ fallback
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "/tmp/no-project/src/myapp/core.clj"
                                         [])))
    ;; Relative path fallback
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/myapp/core.clj"
                                         [])))))

(deftest test-file-path->ns-name-cljs-cljc-extensions
  (testing ".cljs extension stripped"
    (is (= "myapp.ui"
           (#'extract/file-path->ns-name "src/myapp/ui.cljs"))))
  (testing ".cljc extension stripped"
    (is (= "myapp.shared"
           (#'extract/file-path->ns-name "src/myapp/shared.cljc"))))
  (testing ".clj still works"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/myapp/core.clj")))))

(deftest test-file-path->ns-name-dialect-dirs
  (testing "src/clj/ layout strips dialect dir"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/clj/myapp/core.clj"
                                         ["src/clj"]))))
  (testing "src/cljs/ layout strips dialect dir and extension"
    (is (= "myapp.ui"
           (#'extract/file-path->ns-name "src/cljs/myapp/ui.cljs"
                                         ["src/cljs"]))))
  (testing "src/cljc/ layout strips dialect dir and extension"
    (is (= "myapp.shared"
           (#'extract/file-path->ns-name "src/cljc/myapp/shared.cljc"
                                         ["src/cljc"]))))
  (testing "Maven-style layout"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/main/clojure/myapp/core.clj"
                                         ["src/main/clojure"]))))
  (testing "standard src/ with explicit paths"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/myapp/core.clj"
                                         ["src"]))))
  (testing "longest matching prefix wins"
    (is (= "myapp.core"
           (#'extract/file-path->ns-name "src/clj/myapp/core.clj"
                                         ["src" "src/clj"])))))

;; ============================================================
;; Plan integration test with dialect-split layout
;; ============================================================

(deftest test-plan-dialect-split-layout
  (let [root (java.io.File/createTempFile "extract-dialect-test" "")
        _ (.delete root)
        _ (.mkdirs root)
        src-dir (io/file root "src" "clj" "my")]
    (.mkdirs src-dir)
    (spit (io/file root "deps.edn") "{:paths [\"src/clj\"]}")
    (spit (io/file src-dir "app.clj")
          "(ns my.app
  (:require [clojure.string :as str]))

(defn helper [x]
  (str/upper-case x))

(defn distill [x]
  (helper x))
")
    (try
      (let [source (str (.getPath root) "/src/clj/my/app.clj")
            target (str (.getPath root) "/src/clj/my/distillery.clj")
            p (extract/plan {:file source
                             :forms '[distill]
                             :to target
                             :source-paths ["src/clj"]})]
        (testing "target-ns strips dialect dir"
          (is (= "my.distillery" (:target-ns p))))
        (testing "no error"
          (is (nil? (:error p))))
        (testing "preview has correct ns"
          (is (str/includes? (:new-file-preview p) "ns my.distillery"))))
      (finally (delete-recursive! root)))))

(deftest test-execute-dialect-split-layout
  (let [root (java.io.File/createTempFile "extract-dialect-exec" "")
        _ (.delete root)
        _ (.mkdirs root)
        src-dir (io/file root "src" "cljs" "my")]
    (.mkdirs src-dir)
    (spit (io/file root "deps.edn") "{:paths [\"src/cljs\"]}")
    (spit (io/file src-dir "ui.cljs")
          "(ns my.ui
  (:require [clojure.string :as str]))

(defn render-loud [text]
  (str \"<b>\" (str/upper-case text) \"</b>\"))

(defn render-quiet [text]
  (str \"<i>\" (str/lower-case text) \"</i>\"))
")
    (try
      (let [source (str (.getPath root) "/src/cljs/my/ui.cljs")
            target (str (.getPath root) "/src/cljs/my/helpers.cljs")
            result (extract/execute! {:file source
                                      :forms '[render-loud]
                                      :to target
                                      :source-paths ["src/cljs"]})]
        (testing "new file created"
          (is (.exists (io/file target))))
        (testing "new file has correct ns (no cljs. prefix, no .cljs suffix)"
          (let [content (slurp target)]
            (is (str/includes? content "ns my.helpers"))
            (is (not (str/includes? content "cljs.my")))
            (is (not (str/includes? content "helpers.cljs")))))
        (testing "extracted form present"
          (is (str/includes? (slurp target) "defn render-loud")))
        (testing "form removed from source"
          (is (not (str/includes? (slurp source) "defn render-loud"))))
        (testing "non-extracted form remains"
          (is (str/includes? (slurp source) "defn render-quiet")))
        (testing "callerless source does not gain a new require"
          (is (not (str/includes? (slurp source) "my.helpers"))))
        (testing "target keeps the used ClojureScript dependency"
          (is (= ["clojure.string"] (:target-requires result)))
          (is (str/includes? (slurp target) "clojure.string")))
        (testing "receipt reports that no source require was necessary"
          (is (false? (get-in result [:summary :source-require-added])))))
      (finally (delete-recursive! root)))))

;; ============================================================
;; Plan tests (pure, no file mutation)
;; ============================================================

(deftest test-plan-basic
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            p (extract/plan {:file source
                             :forms '[distill refine]
                             :to target})]
        (testing "no error"
          (is (nil? (:error p))))
        (testing "correct form count"
          (is (= 2 (:form-count p))))
        (testing "source and target ns"
          (is (= "my.app" (:source-ns p)))
          (is (= "my.distillery" (:target-ns p))))
        (testing "preview contains new ns name"
          (is (str/includes? (:new-file-preview p) "my.distillery")))
        (testing "preview contains extracted forms"
          (is (str/includes? (:new-file-preview p) "defn distill"))
          (is (str/includes? (:new-file-preview p) "defn refine"))))
      (finally (delete-recursive! root)))))

(deftest test-plan-proves-quoted-var-callers-without-textual-lookalikes
  (let [root (create-temp-project!)
        test-file (io/file root "test/my/app_test.clj")]
    (try
      (.mkdirs (.getParentFile test-file))
      (spit test-file
            (str "(ns my.app-test\n"
                 "  (:require [my.app :as app]))\n"
                 "(deftest aliased-private-var (is (var? #'app/distill)))\n"
                 "(deftest qualified-private-var (is (var? #'my.app/refine)))\n"
                 "(def text \"#'my.app/refine\")\n"
                 ";; #'my.app/distill\n"
                 "(def data '(var my.app/refine))\n"))
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            plan (extract/plan {:file source
                                :forms '[distill refine]
                                :to target})]
        (is (nil? (:error plan)))
        (is (= 2 (count (:quoted-var-references plan))))
        (is (= #{"my.app/distill" "my.app/refine"}
               (set (map :subject (:quoted-var-references plan)))))
        (is (= #{:structural-var-quote}
               (set (map :reference-authority
                         (:quoted-var-references plan))))))
      (finally (delete-recursive! root)))))

(deftest test-plan-missing-form
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            p (extract/plan {:file source
                             :forms '[distill nonexistent]
                             :to target})]
        (testing "error for missing form"
          (is (some? (:error p)))
          (is (str/includes? (:error p) "nonexistent"))))
      (finally (delete-recursive! root)))))

(deftest test-plan-omits-unused-requires
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            p (extract/plan {:file source
                             :forms '[distill]
                             :to target})]
        (testing "new file omits dependencies unused by the moved form"
          (is (empty? (:target-requires p)))
          (is (= ["clojure.string"] (:omitted-target-requires p)))
          (is (not (str/includes? (:new-file-preview p) "clojure.string")))))
      (finally (delete-recursive! root)))))

;; ============================================================
;; Execute tests
;; ============================================================

(deftest test-execute-creates-new-file
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            result (extract/execute! {:file source
                                      :forms '[distill refine]
                                      :to target})]
        (testing "new file created"
          (is (.exists (io/file target))))
        (testing "new file has correct ns"
          (let [content (slurp target)]
            (is (str/includes? content "ns my.distillery"))))
        (testing "new file has the forms"
          (let [content (slurp target)]
            (is (str/includes? content "(defn distill"))
            (is (str/includes? content "(defn refine"))))
        (testing "new file omits unused source requires"
          (let [content (slurp target)]
            (is (not (str/includes? content "clojure.string")))
            (is (empty? (:target-requires result)))))
        (testing "summary correct"
          (is (= 2 (-> result :summary :forms-extracted)))))
      (finally (delete-recursive! root)))))

(deftest test-execute-removes-from-source
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            _ (extract/execute! {:file source
                                 :forms '[distill refine]
                                 :to target})
            source-content (slurp source)]
        (testing "extracted forms removed from source"
          (is (not (str/includes? source-content "(defn distill")))
          (is (not (str/includes? source-content "(defn refine"))))
        (testing "non-extracted forms remain"
          (is (str/includes? source-content "(defn helper"))
          (is (str/includes? source-content "(defn -main"))
          (is (str/includes? source-content "(def config")))
        (testing "source ns still intact"
          (is (str/includes? source-content "ns my.app"))))
      (finally (delete-recursive! root)))))

(deftest test-execute-does-not-consume-the-next-form-without-a-blank-line
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")]
    (try
      (spit source
            "(ns my.app)\n\n(defn move-me [] :moved)\n(defn event-resume-path [event]\n  (:resume-path event))\n")
      (extract/execute! {:file (.getPath source)
                         :forms '[move-me]
                         :to (.getPath target)})
      (let [rewritten (slurp source)]
        (testing "the complete rewritten namespace still parses"
          (is (some? (parser/parse-string-all rewritten))))
        (testing "the adjacent form remains byte-for-byte intact"
          (is (str/includes? rewritten
                             "(defn event-resume-path [event]\n  (:resume-path event))"))))
      (finally (delete-recursive! root)))))

(deftest test-execute-refuses-an-existing-target-without-changing-either-file
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")
        original-source (slurp source)
        original-target "existing target\n"]
    (try
      (spit target original-target)
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[distill refine]
                                      :to (.getPath target)})]
        (is (= :extraction-target-exists (:error-type result)))
        (is (true? (:source-unchanged result)))
        (is (= original-source (slurp source)))
        (is (= original-target (slurp target))))
      (finally (delete-recursive! root)))))

;; Field failure (2026-10-21, invoice refactoring trial): `:extract!` moved a
;; private `defn-` that the source still called. The new file kept `defn-`,
;; so the source's call failed clj-kondo with "is private". The plan had
;; already reported `:missing-required-public-forms`; the CLI executor wrote
;; anyway. The MCP route derives the required public forms by default and
;; refuses an explicit list that omits one; the CLI now does the same.
(defn- create-private-caller-project!
  "A minimized shape of the trial's billing.invoice namespace."
  []
  (let [root (java.io.File/createTempFile "extract-private" "")
        _ (.delete root)
        _ (.mkdirs root)
        src-dir (io/file root "src" "billing")]
    (.mkdirs src-dir)
    (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
    (spit (io/file src-dir "invoice.clj")
          (str "(ns billing.invoice\n"
               "  (:require [clojure.string :as str]))\n\n"
               "(defn- has-email? [customer]\n"
               "  (not (str/blank? (:email customer))))\n\n"
               "(defn- deliver! [customer]\n"
               "  (when (has-email? customer)\n"
               "    [:queued (:email customer)]))\n\n"
               "(defn process [customer]\n"
               "  (deliver! customer))\n"))
    root))

(deftest test-execute-publicizes-a-moved-private-form-that-the-source-still-calls
  (let [root (create-private-caller-project!)
        source (io/file root "src" "billing" "invoice.clj")
        target (io/file root "src" "billing" "delivery.clj")]
    (try
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[deliver! has-email?]
                                      :to (.getPath target)})
            target-text (slurp target)]
        (is (nil? (:error result)))
        (testing "the form the source still calls becomes public"
          (is (str/includes? target-text "(defn deliver! [customer]")))
        (testing "a helper called only by moved forms stays private"
          (is (str/includes? target-text "(defn- has-email? [customer]")))
        (is (= ["deliver!"] (:public-forms result)))
        (is (some? (parser/parse-string-all target-text))))
      (finally (delete-recursive! root)))))

(deftest test-execute-refuses-an-explicit-public-forms-list-that-omits-a-required-form
  (let [root (create-private-caller-project!)
        source (io/file root "src" "billing" "invoice.clj")
        target (io/file root "src" "billing" "delivery.clj")
        original-source (slurp source)]
    (try
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[deliver! has-email?]
                                      :public-forms '[]
                                      :to (.getPath target)})]
        (is (= :required-public-forms-missing (:error-type result)))
        (is (= ["deliver!"] (:missing-public-forms result)))
        (is (true? (:source-unchanged result)))
        (is (true? (:target-unchanged result)))
        (is (= original-source (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

;; Field failure (2026-10-21, found while planning moves into existing
;; namespaces): extracting forms that call each other removed them from the
;; source but wrote neither to the target, and reported success. The
;; topological order omitted every form in a dependency cycle, and every form
;; that depends on one. The same run left `(declare odd-step)` in the source.
(def ^:private mutual-recursion-source
  (str "(ns app.core)\n\n"
       "(declare odd-step later)\n\n"
       "(defn even-step [n]\n"
       "  (if (zero? n) true (odd-step (dec n))))\n\n"
       "(defn odd-step [n]\n"
       "  (if (zero? n) false (even-step (dec n))))\n\n"
       "(defn parity [n] (if (even-step n) :even :odd))\n\n"
       "(defn main [] [(parity 4) (later)])\n\n"
       "(defn later [] :later)\n"))

(defn- mutual-recursion-plan []
  (extract/compile-plan {:file "src/app/core.clj"
                         :source mutual-recursion-source
                         :forms '[even-step odd-step parity]
                         :to "src/app/parity.clj"
                         :target-ns "app.parity"}))

(deftest test-compile-plan-writes-every-form-of-a-dependency-cycle
  (let [plan (mutual-recursion-plan)
        target (:_new-file-content plan)
        index #(str/index-of target %)]
    (is (nil? (:error plan)))
    (testing "every selected form reaches the target"
      (is (index "(defn even-step [n]"))
      (is (index "(defn odd-step [n]"))
      (is (index "(defn parity [n]")))
    (testing "a form used before its definition is declared first"
      (is (index "(declare odd-step)"))
      (is (< (index "(declare odd-step)") (index "(defn even-step [n]"))))
    (testing "the cycle keeps its source order, and its dependent follows it"
      (is (< (index "(defn even-step [n]")
             (index "(defn odd-step [n]")
             (index "(defn parity [n]"))))
    (is (some? (parser/parse-string-all target)))))

(deftest test-compile-candidates-drops-moved-names-from-source-declares
  (let [plan (mutual-recursion-plan)
        {:keys [source]}
        (extract/compile-candidates
          {:source mutual-recursion-source
           :source-file "src/app/core.clj"
           :target-file "src/app/parity.clj"
           :form-ranges (:_form-texts plan)
           :target-source (:_new-file-content plan)
           :target-ns (:target-ns plan)
           :target-alias (:target-alias plan)
           :source-referred-forms (:_source-referred-forms plan)})]
    (is (str/includes? source "(declare later)"))
    (is (not (str/includes? source "odd-step")))
    (is (not (str/includes? source "(defn even-step")))
    (is (str/includes? source "(defn later [] :later)"))))

(deftest test-compile-candidates-removes-a-declare-that-names-only-moved-forms
  (let [source (str "(ns app.core)\n\n"
                    "(declare odd-step)\n\n"
                    "(defn even-step [n]\n"
                    "  (if (zero? n) true (odd-step (dec n))))\n\n"
                    "(defn odd-step [n]\n"
                    "  (if (zero? n) false (even-step (dec n))))\n\n"
                    "(defn main [] (even-step 4))\n")
        plan (extract/compile-plan {:file "src/app/core.clj"
                                    :source source
                                    :forms '[even-step odd-step]
                                    :to "src/app/parity.clj"
                                    :target-ns "app.parity"})
        result (:source (extract/compile-candidates
                          {:source source
                           :source-file "src/app/core.clj"
                           :target-file "src/app/parity.clj"
                           :form-ranges (:_form-texts plan)
                           :target-source (:_new-file-content plan)
                           :target-ns (:target-ns plan)
                           :target-alias (:target-alias plan)
                           :source-referred-forms (:_source-referred-forms plan)}))]
    (is (not (str/includes? result "declare")))
    (is (not (str/includes? result "\n\n\n")))
    (is (str/includes? result "(defn main [] (even-step 4))"))))

(deftest test-execute-rolls-back-the-created-target-when-source-commit-fails
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")
        original-source (slurp source)
        real-atomic-write! file-ops/atomic-write!
        write-count (atom 0)]
    (try
      (let [failure
            (try
              (with-redefs [file-ops/atomic-write!
                            (fn [file content]
                              (if (= 2 (swap! write-count inc))
                                (throw (ex-info "simulated source write failure" {}))
                                (real-atomic-write! file content)))]
                (extract/execute! {:file (.getPath source)
                                   :forms '[distill refine]
                                   :to (.getPath target)}))
              nil
              (catch Exception e e))]
        (is (= :extraction-commit-failed (-> failure ex-data :error-type)))
        (is (true? (-> failure ex-data :source-restored)))
        (is (true? (-> failure ex-data :target-removed)))
        (is (= original-source (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

(deftest test-extraction-receipt-undo-restores-the-original-source
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")
        receipt (io/file root "receipts" "extract.edn")
        original-source (slurp source)]
    (try
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[distill refine]
                                      :to (.getPath target)
                                      :receipt-out (.getPath receipt)})]
        (is (= (.getCanonicalPath receipt) (:receipt-file result)))
        (is (.exists target))
        (is (.exists receipt))
        (let [undo (extract/undo! {:receipt (.getPath receipt)})]
          (is (true? (:ok undo)))
          (is (= original-source (slurp source)))
          (is (not (.exists target))))
        (testing "the same inverse cannot be applied twice"
          (let [second-undo (extract/undo! {:receipt (.getPath receipt)})]
            (is (= :stale-extraction-result (:error-type second-undo)))
            (is (= original-source (slurp source))))))
      (finally (delete-recursive! root)))))

(deftest test-extraction-undo-refuses-a-modified-target-before-writing
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")
        receipt (io/file root "extract.edn")]
    (try
      (extract/execute! {:file (.getPath source)
                         :forms '[distill refine]
                         :to (.getPath target)
                         :receipt-out (.getPath receipt)})
      (let [extracted-source (slurp source)
            changed-target (str (slurp target) "\n;; user change\n")]
        (spit target changed-target)
        (let [undo (extract/undo! {:receipt (.getPath receipt)})]
          (is (= :stale-extraction-result (:error-type undo)))
          (is (true? (:source-unchanged undo)))
          (is (= extracted-source (slurp source)))
          (is (= changed-target (slurp target)))))
      (finally (delete-recursive! root)))))

(deftest test-execute-refuses-a-source-that-changed-after-planning
  (let [root (create-temp-project!)
        source (io/file root "src" "my" "app.clj")
        target (io/file root "src" "my" "distillery.clj")
        real-plan extract/plan
        concurrent-source (str (slurp source) "\n;; concurrent user change\n")]
    (try
      (let [result
            (with-redefs [extract/plan
                          (fn [opts]
                            (let [planned (real-plan opts)]
                              (spit source concurrent-source)
                              planned))]
              (extract/execute! {:file (.getPath source)
                                 :forms '[distill refine]
                                 :to (.getPath target)}))]
        (is (= :stale-extraction-source (:error-type result)))
        (is (true? (:source-unchanged result)))
        (is (= concurrent-source (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

(deftest test-production-shaped-adjacent-extraction-is-parseable-and-reversible
  (let [root (create-temp-project!)
        source (io/file root "src" "fixtures" "extract_adjacent.clj")
        target (io/file root "src" "fixtures" "stages.clj")
        receipt (io/file root "extract-adjacent.edn")
        fixture (slurp (io/file "test" "fixtures" "extract_adjacent.clj"))
        forms (mapv #(symbol (format "stage-%02d" %)) (range 1 16))]
    (try
      (.mkdirs (.getParentFile source))
      (spit source fixture)
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms forms
                                      :to (.getPath target)
                                      :receipt-out (.getPath receipt)})
            future-source (slurp source)
            future-target (slurp target)]
        (is (= 15 (get-in result [:summary :forms-extracted])))
        (is (some? (parser/parse-string-all future-source)))
        (is (some? (parser/parse-string-all future-target)))
        (is (str/includes?
              future-source
              ";; This documented neighbor has no sacrificial blank line before it.\n(defn event-resume-path [event]\n  (:resume-path event))"))
        (is (= 15 (count (re-seq #"\(defn stage-" future-target))))
        (is (true? (:ok (extract/undo! {:receipt (.getPath receipt)}))))
        (is (= fixture (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

(deftest test-execute-adds-require-to-source
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            _ (extract/execute! {:file source
                                 :forms '[distill refine]
                                 :to target})
            source-content (slurp source)]
        (testing "source has new require"
          (is (str/includes? source-content "my.distillery"))))
      (finally (delete-recursive! root)))))

(deftest test-execute-comment-header-moves-with-form
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            _ (extract/execute! {:file source
                                 :forms '[distill refine]
                                 :to target})
            target-content (slurp target)]
        (testing "comment header extracted with form"
          (is (str/includes? target-content ";; Distillery functions"))))
      (finally (delete-recursive! root)))))

(deftest test-execute-topological-order
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/distillery.clj")
            _ (extract/execute! {:file source
                                 :forms '[distill refine]
                                 :to target})
            target-content (slurp target)]
        (testing "distill before refine (refine depends on distill)"
          (is (< (str/index-of target-content "(defn distill")
                 (str/index-of target-content "(defn refine")))))
      (finally (delete-recursive! root)))))

(deftest test-execute-creates-subdirectory
  (let [root (create-temp-project!)]
    (try
      (let [source (str (.getPath root) "/src/my/app.clj")
            target (str (.getPath root) "/src/my/app/distillery.clj")
            _ (extract/execute! {:file source
                                 :forms '[distill]
                                 :to target})]
        (testing "subdirectory created"
          (is (.exists (io/file target))))
        (testing "ns name derived from path"
          (let [content (slurp target)]
            (is (str/includes? content "ns my.app.distillery")))))
      (finally (delete-recursive! root)))))

(deftest test-production-extraction-keeps-only-used-requires-and-avoids-alias-collision
  ;; Minimized from sessionize-sched-killer, 2026-08-10, clj-surgeon-to4.
  (let [root (create-dependency-minimal-project!)
        source (io/file root "src" "fixture" "views.clj")
        target (io/file root "src" "fixture" "views" "schedule.clj")
        receipt (io/file root "dependency-minimal-receipt.edn")]
    (try
      (testing "the field fixture is valid before extraction"
        (is (zero? (:exit (cold-require-result root "fixture.views")))))
      (let [plan (extract/plan {:file (.getPath source)
                                :forms '[day-tab agenda-page]
                                :to (.getPath target)})]
        (testing "the target header contains exactly the six used dependencies"
          (is (= (mapv #(format "fixture.dep%02d" %) (range 1 7))
                 (:target-requires plan)))
          (is (= 11 (count (:omitted-target-requires plan))))
          (is (= 6 (count (re-seq #"\[fixture\.dep" (:_new-file-content plan)))))
          (is (not (str/includes? (:_new-file-content plan)
                                  "fixture.domain.schedule"))))
        (testing "the unused colliding alias does not cause a source require"
          (is (= "schedule2" (:target-alias plan)))
          (is (empty? (:remaining-source-callers plan)))
          (is (empty? (:source-referred-forms plan)))
          (is (false? (:source-require-added plan))))
        (testing "the source is never reported as its own external caller"
          (is (not-any? #(= (.getCanonicalPath source)
                            (.getCanonicalPath (io/file %)))
                        (:callers-to-review plan))))
        (let [result (extract/execute! {:file (.getPath source)
                                        :forms '[day-tab agenda-page]
                                        :to (.getPath target)
                                        :receipt-out (.getPath receipt)})]
          (testing "both generated namespaces load"
            (is (= 2 (get-in result [:summary :forms-extracted])))
            (let [runtime (cold-require-result root
                                               "fixture.views"
                                               "fixture.views.schedule")]
              (is (zero? (:exit runtime)) (:err runtime))))
          (testing "the callerless source has no destination require"
            (is (not (str/includes? (slurp source) "fixture.views.schedule"))))
          (testing "undo restores both paths exactly"
            (is (true? (:ok (extract/undo! {:receipt (.getPath receipt)}))))
            (is (= (dependency-minimal-source) (slurp source)))
            (is (not (.exists target))))))
      (finally (delete-recursive! root)))))

(deftest test-source-caller-gets-one-runtime-valid-refer-entry
  (let [root (java.io.File/createTempFile "extract-source-caller" "")
        _ (.delete root)
        _ (.mkdirs root)
        source (io/file root "src" "fixture" "callers.clj")
        target (io/file root "src" "fixture" "moved.clj")
        receipt (io/file root "caller-receipt.edn")]
    (try
      (.mkdirs (.getParentFile source))
      (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
      (spit source
            "(ns fixture.callers)\n\n(defn moved [] :ok)\n\n(defn caller [] (moved))\n")
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[moved]
                                      :to (.getPath target)
                                      :receipt-out (.getPath receipt)})]
        (is (= [{:owner "caller" :moved-vars ["moved"]}]
               (:remaining-source-callers result)))
        (is (= ["moved"] (:source-referred-forms result)))
        (is (str/includes? (slurp source)
                           "[fixture.moved :as moved :refer [moved]]"))
        (let [runtime (cold-eval-result
                        root
                        "(require 'fixture.callers) (assert (= :ok (fixture.callers/caller)))")]
          (is (zero? (:exit runtime)) (:err runtime)))
        (is (true? (:ok (extract/undo! {:receipt (.getPath receipt)})))))
      (finally (delete-recursive! root)))))

(deftest pure-compile-plan-separates-conservative-movement-from-minimization
  (let [source
        (str "(ns sample.core\n"
             "  (:require ;; keep this side effect\n"
             "   [alpha.side-effects]\n"
             "   [clojure.string :as str]))\n\n"
             "(defn helper [] (str/upper-case \"x\"))\n"
             "(defn moved [] (helper))\n"
             "(defn remains [] (moved))\n")
        input {:file "src/sample/core.clj"
               :source source
               :forms '[helper moved]
               :to "src/sample/extracted.clj"
               :target-ns "sample.extracted"
               :workspace-sources
               {"src/sample/consumer.clj"
                (str "(ns sample.consumer\n"
                     "  (:require [sample.core :as core]))\n"
                     "(def pinned #'core/moved)\n")}}
        minimal (extract/compile-plan (assoc input :require-policy :minimal))
        conservative (extract/compile-plan
                       (assoc input :require-policy :copy-all))
        candidates
        (extract/compile-candidates
          {:source source
           :source-file (:file input)
           :target-file (:to input)
           :form-ranges (:_form-texts conservative)
           :target-source (:_new-file-content conservative)
           :target-ns (:target-ns conservative)
           :target-alias (:target-alias conservative)
           :source-referred-forms (:_source-referred-forms conservative)})]
    (testing "minimal proof still refuses an unproved side-effect require"
      (is (false? (:ok minimal)))
      (is (= :unsupported-require-minimization (:error-type minimal)))
      (is (= :comment-bearing-require-clause (:reason minimal))))

    (testing "copy-all moves exact header syntax without dependency judgment"
      (is (= :copy-all (:require-policy conservative)))
      (is (= 2 (:copied-require-count conservative)))
      (is (= :copied-exactly (:target-requires conservative)))
      (is (str/includes? (:_new-file-content conservative)
                         "(:require ;; keep this side effect"))
      (is (str/includes? (:_new-file-content conservative)
                         "[alpha.side-effects]"))
      (is (= ["helper" "moved"] (:forms-to-extract conservative)))
      (is (= ["moved"] (:source-referred-forms conservative)))
      (is (nil? (:target-alias conservative))
          "copy-all needs no invented alias for a refer-only source edge")
      (is (= ["src/sample/consumer.clj"]
             (:callers-to-review conservative)))
      (is (= ["src/sample/consumer.clj"]
             (mapv :file (:quoted-var-references conservative)))))

    (testing "the complete future namespace pair parses from immutable data"
      (is (str/includes? (:source candidates)
                         "[sample.extracted :refer [moved]]"))
      (is (not (str/includes? (:source candidates) ":as nil")))
      (do
        (is (str/includes? (:target candidates)
                           "(defn moved [] (helper))"))
        (is (str/ends-with? (:_new-file-content conservative) ")\n"))
        (is (not (str/ends-with? (:_new-file-content conservative) "\n\n"))
            "the generated target is formatter-stable at end of file")))))

(deftest pure-compile-plan-can-promote-required-visibility-in-one-pass
  (let [source (str "(ns sample.core)\n\n"
                    "(defn- helper [value] (inc value))\n\n"
                    "(defn keep-me [] (helper 1))\n")
        plan (extract/compile-plan
               {:file "src/sample/core.clj"
                :source source
                :forms '[helper]
                :to "src/sample/moved.clj"
                :target-ns "sample.moved"
                :workspace-sources {}
                :derive-required-public-forms true})]
    (is (= ["helper"] (:required-public-forms plan)))
    (is (= ["helper"] (:public-forms plan)))
    (is (empty? (:missing-required-public-forms plan)))
    (is (str/includes? (:_new-file-content plan) "(defn helper "))))

(deftest test-unsupported-require-minimization-refuses-before-writing
  (let [root (java.io.File/createTempFile "extract-side-effect-require" "")
        _ (.delete root)
        _ (.mkdirs root)
        source (io/file root "src" "fixture" "effects.clj")
        target (io/file root "src" "fixture" "moved.clj")
        original "(ns fixture.effects (:require [fixture.side-effects]))\n\n(defn moved [] :ok)\n"]
    (try
      (.mkdirs (.getParentFile source))
      (spit (io/file root "deps.edn") "{:paths [\"src\"]}\n")
      (spit source original)
      (let [result (extract/execute! {:file (.getPath source)
                                      :forms '[moved]
                                      :to (.getPath target)})]
        (is (= :unsupported-require-minimization (:error-type result)))
        (is (= :side-effect-only-require (:reason result)))
        (is (= original (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

;; ============================================================
;; Callers in other files (decision 9 of
;; docs/plans/move-into-existing-namespace.md: extract! to a new file rewrites
;; callers too)
;; ============================================================

(deftest pure-compile-plan-refers-moved-protocol-methods-the-source-still-calls
  ;; Eval-side report (2026-10-21): protocol methods. Extracting Shape left
  ;; main's (area x) unresolved.
  (let [plan (extract/compile-plan
               {:file "src/app/source.clj"
                :source (str "(ns app.source)\n\n"
                             "(defprotocol Shape\n  (area [this]))\n\n"
                             "(defn main [x] (area x))\n")
                :forms '[Shape]
                :to "src/app/shape.clj"
                :target-ns "app.shape"})]
    (is (= ["area"] (:source-referred-forms plan)))
    (is (= ["Shape" "area"] (:moved-vars plan)))))

(deftest pure-caller-rewrites-for-a-new-namespace
  (let [source "(ns app.source)\n\n(defn f [] 1)\n\n(defn g [] 2)\n"
        plan (extract/compile-plan {:file "src/app/source.clj" :source source :forms '[f]
                                    :to "src/app/fresh.clj" :target-ns "app.fresh"})
        sources {"src/app/source.clj" source
                 "src/app/use.clj" "(ns app.use\n  (:require [app.source :as s]))\n\n(defn h [] [(s/f) (s/g)])\n"
                 "test/app/source_test.clj" "(ns app.source-test\n  (:require [app.source]))\n\n(#'app.source/f)\n"}
        r (extract/with-caller-rewrites plan sources)]
    ;; The sorted insert keeps an existing one-line :require layout.
    (is (= {"src/app/use.clj" (str "(ns app.use\n  (:require [app.fresh :as fresh]\n   [app.source :as s]))\n\n"
                                   "(defn h [] [(fresh/f) (s/g)])\n")
            "test/app/source_test.clj" (str "(ns app.source-test\n  (:require [app.fresh]\n   [app.source]))\n\n"
                                            "(#'app.fresh/f)\n")}
           (:_callers r)))
    (is (= ["src/app/use.clj" "test/app/source_test.clj"] (:callers-to-review r)))
    (is (= [1 1] (mapv :rewritten (:callers-rewritten r))))
    (testing "a caller that cannot be rewritten refuses"
      (let [r (extract/with-caller-rewrites
                plan (assoc sources "dev/user.clj" "(ns user)\n\n(defn go [] (require 'app.source) ((resolve 'app.source/f)))\n"))]
        (is (= :caller-not-rewritable (:error-type r)))
        (is (= [:runtime-require] (mapv :reason (:references r))))))
    (testing "a rewrite that would close a require cycle refuses"
      (let [plan (extract/compile-plan
                   {:file "src/app/source.clj"
                    :source "(ns app.source\n  (:require [app.use :as u]))\n\n(defn f [] (u/k))\n"
                    :forms '[f] :to "src/app/fresh.clj" :target-ns "app.fresh"
                    :require-policy :minimal})
            r (extract/with-caller-rewrites
                plan {"src/app/use.clj" "(ns app.use\n  (:require [app.source :as s]))\n\n(defn k [] 1)\n(defn h [] (s/f))\n"})]
        (is (= :circular-require (:error-type r)))
        (is (= ["app.fresh" "app.use" "app.fresh"] (:cycle r)))))))

(defn- create-caller-project!
  "The temp project with a deps.edn, an aliased caller, and a test that
  reaches a moved form through a fully qualified var quote."
  []
  (let [root (create-temp-project!)]
    (spit (io/file root "deps.edn") "{:paths [\"src\"] :aliases {:test {:extra-paths [\"test\"]}}}\n")
    (.mkdirs (io/file root "test" "my"))
    (spit (io/file root "src" "my" "cli.clj")
          "(ns my.cli\n  (:require [my.app :as app]))\n\n(defn run [] [(app/distill 1) (app/helper 2)])\n")
    (spit (io/file root "test" "my" "app_test.clj")
          "(ns my.app-test\n  (:require\n   [clojure.test :refer [deftest is]]\n   [my.app]))\n\n(deftest refines (is (#'my.app/refine \"x\")))\n")
    root))

(deftest test-execute-rewrites-callers-and-undo-restores-them
  (let [root (create-caller-project!)
        files (mapv #(io/file root %) ["src/my/app.clj" "src/my/cli.clj" "test/my/app_test.clj"])
        target (io/file root "src" "my" "distillery.clj")
        receipt (io/file root "extract.edn")
        before (mapv slurp files)]
    (try
      (let [result (extract/execute! {:file (.getPath (first files))
                                      :forms '[distill refine]
                                      :to (.getPath target)
                                      :receipt-out (.getPath receipt)
                                      :lint (fn [_] {:checked false})})]
        (is (str/includes? (slurp (nth files 1)) "[(distillery/distill 1) (app/helper 2)]"))
        (is (str/includes? (slurp (nth files 2)) "(#'my.distillery/refine \"x\")"))
        (is (= ["src/my/cli.clj" "test/my/app_test.clj"] (:callers-to-review result)))
        (is (= 2 (count (:callers-rewritten result))))
        (testing "undo restores every file and removes the target"
          (is (true? (:ok (extract/undo! {:receipt (.getPath receipt)}))))
          (is (= before (mapv slurp files)))
          (is (not (.exists target)))))
      (finally (delete-recursive! root)))))

(deftest test-execute-refuses-a-caller-that-changed-after-planning
  (let [root (create-caller-project!)
        source (io/file root "src" "my" "app.clj")
        caller (io/file root "src" "my" "cli.clj")
        target (io/file root "src" "my" "distillery.clj")
        before (slurp source)
        real-plan extract/plan]
    (try
      (let [result (with-redefs [extract/plan (fn [opts]
                                                (let [p (real-plan opts)]
                                                  (spit caller (str (slurp caller) ";; edited\n"))
                                                  p))]
                     (extract/execute! {:file (.getPath source) :forms '[distill refine]
                                        :to (.getPath target) :lint (fn [_] {:checked false})}))]
        (is (= :stale-extraction-source (:error-type result)))
        (is (= ["src/my/cli.clj"] (:changed result)))
        (is (= before (slurp source)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

(deftest test-execute-lint-regression-restores-every-file
  (let [root (create-caller-project!)
        files (mapv #(io/file root %) ["src/my/app.clj" "src/my/cli.clj" "test/my/app_test.clj"])
        target (io/file root "src" "my" "distillery.clj")
        before (mapv slurp files)
        calls (atom 0)
        lint (fn [_] (if (= 1 (swap! calls inc))
                       {:checked true :findings []}
                       {:checked true :findings [{:filename "src/my/cli.clj" :type :unresolved-var
                                                  :level :warning :message "Unresolved var: x/y"}]}))]
    (try
      (let [result (extract/execute! {:file (.getPath (first files)) :forms '[distill refine]
                                      :to (.getPath target) :lint lint})]
        (is (= :lint-regression (:error-type result)))
        (is (true? (:restored result)))
        (is (= before (mapv slurp files)))
        (is (not (.exists target))))
      (finally (delete-recursive! root)))))

;; Field regression (2026-10-21): extract required lint-gate, which used
;; babashka.fs and babashka.process. The MCP server's classpath (deps.edn
;; :deps) has neither, so the server could not load extract and never became
;; ready. Every namespace the MCP server can reach must load on that classpath.
(deftest the-mcp-server-classpath-loads-the-extraction-namespaces
  (let [{:keys [exit err]} (shell/sh "clojure" "-M" "-e"
                                     (str "(require 'clj-surgeon.extract 'clj-surgeon.lint-gate "
                                          "'clj-surgeon.move-callers 'clj-surgeon.move-into)"))]
    (is (zero? exit) err)))
