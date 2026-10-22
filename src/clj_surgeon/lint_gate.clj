(ns clj-surgeon.lint-gate
  "The clj-kondo gate around a multi-file write: lint every written file
  before and after, and name the blocking findings the write introduced.
  Plain JVM Clojure: the MCP server loads it through extract, and its
  classpath has no Babashka libraries."
  (:require
   [clj-surgeon.diagnostic-delta :as diagnostic-delta]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]))

(defn- on-path?
  "Is an executable named `command` on PATH?"
  [command]
  (some #(.canExecute (io/file % command))
        (str/split (or (System/getenv "PATH") "") #":")))

(defn clj-kondo-lint
  "Lint `files` together with clj-kondo from PATH. Returns {:checked false}
  when clj-kondo is not installed."
  [files]
  (if-not (on-path? "clj-kondo")
    {:checked false :reason "clj-kondo is not installed"}
    (let [result (apply shell/sh "clj-kondo" "--config" "{:output {:format :edn}}"
                        "--lint" (map str files))
          parsed (try (edn/read-string (:out result)) (catch Exception _ nil))]
      (if (map? parsed)
        {:checked true :findings (vec (:findings parsed))}
        {:checked false :reason "clj-kondo output could not be read"}))))

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
