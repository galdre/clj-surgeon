(ns clj-surgeon.receipt-mac-test
  "Signing CLI undo receipts (fence re-review 2026-10-22, N3)."
  (:require
   [clj-surgeon.receipt-mac :as receipt-mac]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(defn- with-home [f]
  (let [home (.toFile (java.nio.file.Files/createTempDirectory "receipt-home" (make-array java.nio.file.attribute.FileAttribute 0)))
        previous (System/getProperty "user.home")]
    (try
      (System/setProperty "user.home" (str home))
      (f home)
      (finally
        (System/setProperty "user.home" previous)
        (doseq [file (reverse (file-seq home))] (.delete file))))))

(deftest a-signed-receipt-verifies-and-an-altered-one-does-not
  (with-home
    (fn [_]
      (let [signed (receipt-mac/sign {:kind :move-into :files [{:path "/a" :original "x"}]})]
        (is (receipt-mac/authentic? signed))
        (is (not (receipt-mac/authentic? (assoc-in signed [:files 0 :path] "/b"))))
        (is (not (receipt-mac/authentic? (dissoc signed :mac))))))))

(deftest the-key-is-created-owner-only-and-a-readable-key-refuses
  (with-home
    (fn [home]
      (receipt-mac/sign {:kind :x})
      (let [key-file (io/file home ".local" "state" "clj-surgeon" "receipt-key")]
        (is (= "rw-------" (java.nio.file.attribute.PosixFilePermissions/toString
                             (java.nio.file.Files/getPosixFilePermissions (.toPath key-file) (make-array java.nio.file.LinkOption 0)))))
        (testing "a key other users can read refuses to sign"
          (java.nio.file.Files/setPosixFilePermissions (.toPath key-file)
                                                       (java.nio.file.attribute.PosixFilePermissions/fromString "rw-r--r--"))
          (is (thrown-with-msg? Exception #"receipt key" (receipt-mac/sign {:kind :x}))))))))

(deftest an-empty-key-refuses-with-a-typed-error
  (with-home
    (fn [home]
      (let [key-file (io/file home ".local" "state" "clj-surgeon" "receipt-key")]
        (.mkdirs (.getParentFile key-file))
        (java.nio.file.Files/createFile (.toPath key-file)
                                        (into-array java.nio.file.attribute.FileAttribute
                                                    [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute
                                                       (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------"))]))
        (is (= :receipt-key-invalid
               (try (receipt-mac/sign {:kind :x}) nil
                    (catch clojure.lang.ExceptionInfo e (:error-type (ex-data e))))))))))
