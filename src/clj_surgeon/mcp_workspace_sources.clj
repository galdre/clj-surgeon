(ns clj-surgeon.mcp-workspace-sources
  "One deterministic source universe shared by extraction planning and apply."
  (:require
   [clj-surgeon.quoted-var-refs :as quoted-var-refs]
   [clojure.string :as str])
  (:import
   (java.nio.file Path Paths)))

(def source-file-pattern #".*\.clj[sc]?$")

;; @spec MCP-OP-PLAN-001
(defn read-all
  "Return canonical source paths and exact bytes for one confined workspace."
  [^Path root]
  (->> (file-seq (.toFile root))
       (filter #(.isFile %))
       (filter #(re-matches source-file-pattern (.getName %)))
       (remove #(str/includes? (.getPath %) "/.git/"))
       (map (fn [file] [(.getCanonicalPath file) (slurp file)]))
       (into (sorted-map))))

(defn read-declared
  "Canonical source paths and exact bytes under the workspace's declared
  source roots, or under `source-roots` (workspace-relative) when given,
  confined to the real root, with build and tool directories skipped. Caller
  discovery reads this universe: a file outside it is never rewritten."
  ([^Path root] (read-declared root nil {}))
  ([^Path root source-roots] (read-declared root source-roots {}))
  ([^Path root source-roots {:keys [max-files max-bytes]}]
   (if-let [unusable (some->> source-roots (quoted-var-refs/unusable-source-roots (str root)) not-empty)]
     {:ok false
      :error-type :invalid-source-roots
      :error "Each source root must be a directory inside the workspace"
      :source-roots unusable
      :source-unchanged true}
     (let [files (if source-roots
                   (quoted-var-refs/workspace-files (str root) source-roots)
                   (quoted-var-refs/workspace-files (str root)))
           ;; Bounds come from the directory listing, before any file is read.
           bytes (reduce + 0 (map #(.length ^java.io.File %) files))]
       (if (or (and max-files (> (count files) max-files))
               (and max-bytes (> bytes max-bytes)))
         {:ok false
          :error-type :workspace-scan-budget-exceeded
          :error "The source roots hold more files or bytes than one request may read"
          :files (count files)
          :bytes bytes
          :limit {:files max-files :bytes max-bytes}
          :next-step "Narrow the scan with source_roots, then retry"
          :source-unchanged true}
         (->> files
              (map (fn [file] [(.getCanonicalPath file) (slurp file)]))
              (into (sorted-map))))))))

(defn relative-paths
  "Map each canonical source path to its project-relative spelling."
  [^Path root sources]
  (let [canonical-root (.toRealPath root (make-array java.nio.file.LinkOption 0))]
    (into {}
          (map (fn [source-path]
                 [source-path
                  (-> canonical-root
                      (.relativize (Paths/get source-path (make-array String 0)))
                      str)]))
          (keys sources))))
