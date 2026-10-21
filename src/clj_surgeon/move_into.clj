(ns clj-surgeon.move-into
  "Move top-level forms into an existing namespace file.

   Plan: docs/plans/move-into-existing-namespace.md. Everything here is pure:
   source texts in, complete future texts or a typed refusal out. The CLI
   shell in clj-surgeon.extract reads the files, writes the result
   atomically, lints it, and rolls back on a new error."
  (:require
   [clj-surgeon.forms :as forms]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

;; ============================================================
;; Target model
;; ============================================================

(defn- token-string [zloc]
  (when zloc
    (if (= :meta (n/tag (z/node zloc)))
      (some-> zloc z/down z/rightmost z/string)
      (z/string zloc))))

(defn- comment-start
  "0-based index of the first line of the `;` comment block directly above
  the form that starts on 1-based `form-line`."
  [lines form-line]
  (loop [i (- form-line 2)]
    (cond
      (neg? i) 0
      (str/starts-with? (str/trim (nth lines i "")) ";") (recur (dec i))
      :else (inc i))))

(defn top-level-forms
  "Every top-level list form of `source`: its type, name, dispatch value (for
  defmethod), 1-based start and end rows, and the 0-based index of its first
  attached comment line."
  [source]
  (let [lines (vec (str/split-lines source))]
    (->> (z/of-string source {:track-position? true})
         (iterate z/right)
         (take-while some?)
         (filter z/list?)
         (mapv (fn [zloc]
                 (let [head (z/down zloc)
                       name-loc (some-> head z/right)
                       {:keys [row end-row]} (meta (z/node zloc))]
                   {:type (some-> head z/string)
                    :name (token-string name-loc)
                    :name-symbol? (and (some? name-loc)
                                       (symbol? (try (z/sexpr name-loc)
                                                     (catch Exception _ nil))))
                    :dispatch (some-> name-loc z/right z/string)
                    :line row
                    :end-line end-row
                    :comment-start (comment-start lines row)}))))))

(defn- defining? [project-aliases {:keys [type]}]
  (and type
       (not= "declare" type)
       (some? (forms/spec-with-project-aliases project-aliases type))))

(defn- typed-anchor [{:keys [type name dispatch]} same-name-count]
  (if (and (= "defmethod" type) dispatch)
    (str "defmethod " name " " dispatch)
    (if (> same-name-count 1) (str type " " name) name)))

;; ============================================================
;; Anchors
;; ============================================================

(def anchor-keys [:above :below :at-start :at-end])

(defn- parse-named-anchor
  "\"render\", \"defmulti render\", or \"defmethod render :card\"."
  [text]
  (let [parts (str/split (str/trim (str text)) #"\s+" 3)]
    (case (count parts)
      1 {:name (first parts)}
      2 {:type (first parts) :name (second parts)}
      {:type (first parts) :name (second parts) :dispatch (nth parts 2)})))

(defn resolve-anchor
  "Resolve exactly one of :above, :below, :at-start, :at-end against the
  target's definitions. Returns {:position :above|:below|:at-start|:at-end
  ...the anchored form...} or a refusal."
  [target anchor project-aliases]
  (let [given (filterv #(some? (get anchor %)) anchor-keys)]
    (if (not= 1 (count given))
      {:ok false
       :error-type :anchor-required
       :error "Name exactly one of --above FORM, --below FORM, --at-start, --at-end"
       :given (mapv #(str "--" (name %)) given)}
      (let [position (first given)]
        (if (#{:at-start :at-end} position)
          {:ok true :position position}
          (let [wanted (parse-named-anchor (get anchor position))
                all-forms (top-level-forms target)
                definitions (filterv #(defining? project-aliases %) all-forms)
                by-name (filterv #(= (:name wanted) (:name %)) definitions)
                matches (filterv #(and (or (nil? (:type wanted))
                                           (= (:type wanted) (:type %)))
                                       (or (nil? (:dispatch wanted))
                                           (= (:dispatch wanted) (:dispatch %))))
                                 by-name)]
            (case (count matches)
              1 (assoc (first matches) :ok true :position position)
              0 (let [unrecognized (some #(when (and (= (:name wanted) (:name %))
                                                     (:name-symbol? %)
                                                     (not (defining? project-aliases %)))
                                            %)
                                         all-forms)]
                  (cond->
                    {:ok false
                     :error-type :anchor-not-found
                     :error (str "The target defines no form named "
                                 (get anchor position))
                     :anchor (get anchor position)
                     :alternatives ["--at-start" "--at-end"]}
                    unrecognized
                    (assoc :unrecognized-macro (:type unrecognized)
                           :config-entry {:aliases {(:type unrecognized) {:kind :def}}}
                           :next-step (str "Declare " (:type unrecognized)
                                           " under :aliases in .clj-surgeon.edn so clj-surgeon "
                                           "recognizes it as a defining form, or anchor with "
                                           "--at-start or --at-end"))))
              {:ok false
               :error-type :ambiguous-anchor
               :error (str "The anchor " (get anchor position)
                           " matches " (count matches) " forms")
               :anchor (get anchor position)
               :matches (mapv (fn [form]
                                {:anchor (typed-anchor form (count matches))
                                 :type (:type form)
                                 :line (:line form)})
                              matches)})))))))

;; ============================================================
;; Placement
;; ============================================================

(defn- line-chunks
  "Split text into lines that keep their terminators."
  [text]
  (let [chunks (vec (str/split text #"(?<=\n)" -1))]
    (if (and (seq chunks) (= "" (peek chunks))) (pop chunks) chunks)))

(defn- with-final-newline [s]
  (if (or (str/blank? s) (str/ends-with? s "\n")) s (str s "\n")))

(defn place-block
  "Insert `block` (complete source of the moved forms, no surrounding blank
  lines) into `target` at the resolved anchor, one blank line on each side."
  [target anchor project-aliases block]
  (let [resolved (resolve-anchor target anchor project-aliases)]
    (if-not (:ok resolved)
      resolved
      (let [chunks (line-chunks target)
            ns-form (first (filter #(= "ns" (:type %)) (top-level-forms target)))
            split-at-line (fn [index]
                            [(apply str (subvec chunks 0 index))
                             (apply str (subvec chunks index))])
            after (fn [end-row]
                    (let [[prefix suffix] (split-at-line end-row)]
                      (str (with-final-newline prefix) "\n" block "\n"
                           (if (str/blank? suffix) "" suffix))))
            result
            (case (:position resolved)
              :below (after (:end-line resolved))
              :at-start (after (:end-line ns-form))
              :above (let [[prefix suffix] (split-at-line (:comment-start resolved))]
                       (str prefix block "\n\n" suffix))
              :at-end (str (with-final-newline (str/trimr target)) "\n" block "\n"))]
        {:ok true :target result :anchor resolved}))))
