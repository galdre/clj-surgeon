(ns clj-surgeon.receipt-mac
  "Sign CLI undo receipts with a per-user secret, so undo restores only
  receipts this machine's clj-surgeon wrote. A receipt names files and the
  bytes to write into them; an unsigned or altered one could overwrite any
  file the user can write. Plain JVM Clojure, loadable on the MCP server's
  classpath."
  (:require
   [clojure.java.io :as io]
   [clojure.walk :as walk])
  (:import
   (java.nio.file Files LinkOption)
   (java.nio.file.attribute FileAttribute PosixFilePermissions)
   (java.security MessageDigest SecureRandom)
   (javax.crypto Mac)
   (javax.crypto.spec SecretKeySpec)))

(defn- key-file []
  (io/file (System/getProperty "user.home") ".local" "state" "clj-surgeon" "receipt-key"))

(defn- hex [^bytes bs]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bs)))

(defn- owner-only? [f]
  (try
    (let [perms (PosixFilePermissions/toString
                  (Files/getPosixFilePermissions (.toPath f) (make-array LinkOption 0)))]
      (= "------" (subs perms 3)))
    (catch UnsupportedOperationException _ true)))

(defn- secret
  "The per-user signing secret. It is created owner-only in one step, and a
  key other users can read refuses: they could forge receipts with it."
  []
  (let [f (key-file)]
    (when-not (.exists f)
      (.mkdirs (.getParentFile f))
      (let [bs (byte-array 32)
            path (.toPath f)]
        (.nextBytes (SecureRandom.) bs)
        (try
          (Files/createFile path (into-array FileAttribute
                                             [(PosixFilePermissions/asFileAttribute
                                                (PosixFilePermissions/fromString "rw-------"))]))
          (catch UnsupportedOperationException _
            (Files/createFile path (make-array FileAttribute 0))))
        (spit f (hex bs))))
    (when-not (owner-only? f)
      (throw (ex-info (str "The receipt key " f " is readable by other users; restrict it to its owner")
                      {:error-type :receipt-key-insecure :file (str f)})))
    (let [key (slurp f)]
      (when (< (count key) 32)
        (throw (ex-info (str "The receipt key " f " is empty or truncated; delete it to create a new one")
                        {:error-type :receipt-key-invalid :file (str f)})))
      key)))

(defn- canonical
  "The receipt with every map sorted, so its printed form does not depend on
  map ordering after an EDN round trip."
  [receipt]
  (pr-str (walk/postwalk #(if (map? %) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b)))) %) %)
                         (dissoc receipt :mac))))

(defn- mac [receipt]
  (let [m (Mac/getInstance "HmacSHA256")]
    (.init m (SecretKeySpec. (.getBytes ^String (secret) "UTF-8") "HmacSHA256"))
    (hex (.doFinal m (.getBytes ^String (canonical receipt) "UTF-8")))))

(defn sign
  "The receipt with its :mac."
  [receipt]
  (assoc receipt :mac (mac receipt)))

(defn authentic?
  "Did this machine's clj-surgeon write this receipt, unchanged?"
  [receipt]
  (and (string? (:mac receipt))
       (MessageDigest/isEqual (.getBytes ^String (:mac receipt) "UTF-8")
                              (.getBytes ^String (mac receipt) "UTF-8"))))

(defn refusal
  "The undo refusal for a receipt that is not authentic."
  []
  {:ok false
   :error-type :receipt-not-authentic
   :error "The receipt is unsigned or was changed after the move wrote it; nothing was restored"
   :source-unchanged true
   :target-unchanged true
   :files-unchanged true})
