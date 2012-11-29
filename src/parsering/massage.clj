(ns parsering.massage
  (:use [parsering.lexer :only [split-namespace-elements parse-proto-file]]
        [parsering.common]))

(defn extract-options
  "Extracts the options from a proto-file record and puts it in a map"
  [not-messages]
  (->> not-messages
       (filter #(= :option (:type %)))
       (map #(hash-map (:key %) (dissoc % :key :type)))
       (apply merge)))

(defn split-headers-and-messages
  [file-record]
  (let [parts (group-by #(contains? #{:message :enum} (:type %)) file-record)]
    {:messages (get parts true)
     :meta (get parts false)}))

(defn flatten-declarations
  "Goes through a list of records of :type :message and namespaces each. Also repeats the same thing for nested messages, with more specific namespace of enclosing type. Applies the same options"
  [messages package options]
  (letfn [(fully-namespace [r]
            (-> (assoc r
                  :full-name (->> (:name r)
                                  (conj (split-namespace-elements package))
                                  (reduce #(str %1 "." %2))))
                (dissoc :name)))]
    (let [these-msgs (map fully-namespace messages)
          embeddeds (->> these-msgs
                         (map #(vector (:full-name %)
                                       (concat (:nesteds %)
                                               (:enums %))))
                         (map #(flatten-declarations (second %)
                                                     (first %)
                                                     options))
                         (apply concat))]
      (concat (map #(dissoc % :nesteds :enums) these-msgs) embeddeds))))

(defn resolve-symbol-names
  "Goes through a message, finds symbol references in the message members and tries to resolve them using the nearest symbol in the name space"
  [message-record ns-symbols]
  (let [rmsgns (->> (:full-name message-record) ;; reverse message namespace elements
                    split-namespace-elements
                    reverse)
        limit (count rmsgns)]
    (letfn [(test-symbol [name]
              (contains? ns-symbols name))
            (qualify-symbol [member-record]
              (println "\n")
              (let [member-parts (-> (:member-type member-record)
                                     split-namespace-elements
                                     reverse)]
                (loop [i 0]
                  (let [tested (->> (concat member-parts
                                            (take i rmsgns))
                                    reverse
                                    (reduce #(str %1 "." %2)))]
                    (println (str "testing: " tested))
                    (cond
                     (test-symbol tested) (assoc member-record :full-name tested)
                     (< limit i) (assoc member-record :full-name "UNKNOWN")
                     :else (recur (inc i)))))))
            (update-symbol-ref [member-record]
              (if (:member-is-simple-type member-record)
                member-record
                (qualify-symbol member-record)))]
      (->> (:message-members message-record)
           (map update-symbol-ref)
           (assoc message-record :message-members)))))

(defn load-proto-file
  "Loads a protobuf file, resolves imports, does massaging on the file structure to make it into something that can be used for code-gen, returns a map with keys as fully qualified names and values maps with definitions"
  [filename resolver]
  (->> (parse-proto-file filename resolver)
       (map #(let [{:keys [meta messages]} (split-headers-and-messages (:contents %))
                   package (:package %)]
               {:meta meta
                :messages messages
                :package package}))
       (map #(assoc %
               :options (extract-options (:meta %))))
       (map #(let [{:keys [messages options package]} %]
               (flatten-declarations messages package options)))
       (apply concat '())
       (reduce #(assoc %1 (:full-name %2) %2) {})))



;; lexer.clj
;; (def fsr (parsering.core/create-fs-resolver "resources"))
;; (-> (parsering.lexer/parse-proto-file "test2.proto" fsr) first :contents split-headers-and-messages pprint)


