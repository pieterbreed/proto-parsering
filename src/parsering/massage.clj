(ns parsering.massage
  (:use [parsering.lexer :only [split-namespace-elements parse-proto-file]]))

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

(defn load-proto-file
  "Loads a protobuf file, resolves imports, does massaging on the file structure to make it into something that can be used for code-gen"
  [filename resolver]
  (-> (lex/parse-proto-file filename resolver)
      (

;; lexer.clj
;; (def fsr (parsering.core/create-fs-resolver "resources"))
;; 


