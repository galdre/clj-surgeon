(ns clj-surgeon.lint-gate
  "The clj-kondo gate around a multi-file write: lint every written file
  before and after, and name the blocking findings the write introduced.
  Plain JVM Clojure: the MCP server loads it through extract, and its
  classpath has no Babashka libraries."
  (:require
   [clj-surgeon.diagnostic-delta :as diagnostic-delta]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- on-path?
  "Is an executable named `command` on PATH?"
  [command]
  (some #(.canExecute (io/file % command))
        (str/split (or (System/getenv "PATH") "") #":")))

(def ^:private kondo-command
  ["clj-kondo" "--config" "{:output {:format :edn}}" "--lint"])

(defn clj-kondo-lint
  "Lint `files` together with clj-kondo from PATH, as one argv with no
  shell, within `timeout-ms`. Returns {:checked true :findings [...]}, or
  {:checked false :reason} when clj-kondo is not installed, its output cannot
  be read, or it does not finish in time (then also :timed-out true, and the
  process is destroyed). `command` replaces the argv before the files; `dir`
  is the working directory, so clj-kondo uses that project's config and
  cache rather than the caller's."
  ([files] (clj-kondo-lint files {}))
  ([files {:keys [command timeout-ms dir] :or {command kondo-command timeout-ms 120000}}]
   (if-not (on-path? (first command))
     {:checked false :reason (str (first command) " is not installed")}
     (let [out (java.io.File/createTempFile "clj-surgeon-lint" ".edn")
           builder (doto (ProcessBuilder. ^java.util.List (vec (concat command (map str files))))
                     (cond-> dir (.directory (io/file dir)))
                     (.redirectOutput out)
                     (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD))
           process (.start builder)]
       (try
         (if-not (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
           (do (.destroyForcibly process)
               {:checked false :timed-out true
                :reason (str (first command) " did not finish in " timeout-ms " ms")})
           (let [parsed (try (edn/read-string (slurp out)) (catch Exception _ nil))]
             (if (map? parsed)
               {:checked true :findings (vec (:findings parsed))}
               {:checked false :reason "clj-kondo output could not be read"})))
         (finally (.delete out)))))))

(def ^:private blocking-types
  "Findings that block at any level: clj-kondo reports a reference to a Var
  or namespace that does not exist as a warning, and a move must never leave
  one behind."
  #{:unresolved-var :unresolved-namespace :unresolved-symbol})

(defn- blocking? [{:keys [level type]}]
  (or (= :error (keyword level))
      (contains? blocking-types (keyword type))))

(defn new-blocking-findings
  "Blocking findings in `after` that `before` does not have, ignoring
  position: every error, and every unresolved reference at any level."
  [before after]
  (let [errors #(filter blocking? %)
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
