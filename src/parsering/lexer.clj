(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]
        [parsering.common]
        [parsering.parser :as parser]
        [clojure.tools.cli :only [cli]]))

(defn split-namespace-elements
  "splits a namespace value string into component elements (separated by '.')"
  [s]
  (run (either (let->> [fst (many1 (token #(not= \. %)))
                        rst (many (>> (char \.)
                                      (many1 (token #(not= \. %)))))]
                       (always (apply vector
                                      (conj (map #(apply str %) rst)
                                            (apply str fst)))))
               (always []))
       s))
                        

(defparser match-keywords [& keywords]
  (letfn [(match-keyword-value [kw]
            (some #(= kw %) keywords))]
    (token #(and (= :keyword (:type %))
                 (match-keyword-value (:value %))))))

(defparser match-symbol []
  (token #(and (= :symbol (:type %)))))

(defparser match-string-value []
  (token #(and (= :value (:type %))
               (= :string (:value-type %)))))

(defparser match-int-value []
  (token #(and (= :value (:type %))
               (= :int (:value-type %)))))

(defparser match-package []
  (let->>
   [package
    (either
     (let->> [_ (match-keywords :package)
              pac (match-symbol)
              _ (match-keywords :semicolon)]
             (always (:value pac)))
     (always ""))]
   (always {:type :package
            :value (split-namespace-elements package)})))

(defparser match-option []
  (let->> [_ (match-keywords :option)
           option-name (match-symbol)
           _ (match-keywords :equals)
           option-value (choice (let->> [s (match-symbol)]
                                        (always {:type :symbol
                                                 :value (:value s)}))
                                (let->> [i (match-int-value)]
                                        (always {:type :int
                                                 :value (:value i)}))
                                (let->> [s (match-string-value)]
                                        (always {:type :string
                                                 :value (:value s)})))
           _ (match-keywords :semicolon)]
          (always {:type :option
                   :key (:value option-name)
                   :value-type (:type option-value)
                   :value (:value option-value)})))
          
(defparser match-import []
  (let->> [_ (match-keywords :import)
           location (match-string-value)
           _ (match-keywords :semicolon)]
          (always {:type :import
                   :value (:value location)})))

(defparser match-enum []
  (let->> [_ (match-keywords :enum)
           name (match-symbol)
           _ (match-keywords :open-curly)
           enums (many1 (let->> [enumname (match-symbol)
                                 _ (match-keywords :equals)
                                 enumvalue (match-int-value)
                                 _ (match-keywords :semicolon)]
                                (always {:name (:value enumname)
                                         :value (:value enumvalue)})))
           _ (match-keywords :close-curly)]
          (always {:type :enum
                   :name (:value name)
                   :values (vec enums)})))

(defparser match-message-member []
  (let->> [modifier (choice (match-keywords :optional)
                            (match-keywords :required)
                            (match-keywords :repeated))
           type (either (attempt (let->> [kw (match-keywords :double :float
                                                             :int32 :int64
                                                             :uint32 :uint64
                                                             :sint32 :sint64
                                                             :fixed32 :fixed64
                                                             :sfixed32 :sfixed64
                                                             :bool
                                                             :string
                                                             :bytes
                                                             :enum)]
                                         (always {:member-type (:value kw)
                                                  :member-is-simple-type true})))
                        (let->> [s (either (let->> [_ (match-keywords :point)
                                                    s (match-symbol)]
                                                   (always {:fully-qualified true
                                                            :value (:value s)}))
                                           (let->> [s (match-symbol)]
                                                   (always {:fully-qualified false
                                                            :value (:value s)})))]
                                (always {:member-type (:value s)
                                         :fully-qualified (:fully-qualified s)
                                         :member-is-simple-type false})))
           name (choice (match-symbol)
                        (let->> [kw (token #(= :keyword (:type %)))]
                                (always {:type :symbol
                                         :value (name (:value kw))})))
           _ (match-keywords :equals)
           position (match-int-value)
           option (either (let->> [_ (match-keywords
                                       :open-bracket)
                                    kw (match-symbol)
                                    _ (match-keywords
                                       :equals)
                                    v (choice (match-symbol)
                                              (match-int-value)
                                              (match-string-value))
                                    _ (match-keywords
                                       :close-bracket)]
                                   (always {:name (:value kw)
                                            :value (:value v)}))
                          (always nil))
           _ (match-keywords :semicolon)]
          (always {:type :message-member
                   :modifier (:value modifier)
                   :member-type (:member-type type)
                   :member-is-simple-type (:member-is-simple-type type)
                   :member-is-fully-qualified (get type :fully-qualified false)
                   :name (:value name)
                   :option option
                   :tag (:value position)})))

; forward declaration so that messages may have other messages in them
(declare match-message)

(defparser match-message-item []
  (choice (let->> [msg (match-message)]
                  (always {:type :nesteds
                           :value msg}))
          (let->> [item (match-message-member)]
                  (always {:type :message-members
                           :value item}))
          (let->> [enum (match-enum)]
                  (always {:type :enums
                           :value enum}))))

(defn group-message-items [many-items]
  (letfn [(get-values [xs] (vec (map :value xs)))]
    (->> (group-by :type many-items)
         (map #(hash-map (first %)
                         (get-values (second %))))
         (apply merge))))

(defparser match-message []
  (let->> [_ (match-keywords :message)
           name (match-symbol)
           _ (match-keywords :open-curly)
           items (many (match-message-item))
           _ (match-keywords :close-curly)]
          (let [grouped (group-message-items items)]
          (always (merge
                   {:type :message
                    :name (:value name)}
                   {:message-members (get grouped :message-members [])}
                   {:enums (get grouped :enums [])}
                   {:nesteds (get grouped :nesteds [])})))))

(defparser match-proto-file []
  (let->> [p (match-package)
           contents (many (choice (match-option)
                                  (match-message)
                                  (match-import)
                                  (match-enum)))
           _ (eof)]
          (always {:type :proto-file
                   :package (:value p)
                   :contents contents})))

(defn extract-file-options
  "extracts the options in the file into a structure that can be embedded into a message struc"
  [fr]
  (->> fr
       :contents
       (filter #(= :option (:type %)))
       (map #(hash-map (:key %)
                       (select-keys % [:value-type :value])))
       (apply merge {})))

(defn namespace-enum-record
  "Adds a full namespace property to a enum record"
  [er ns]
  {:pre [(and (vector? ns)
              (map? er)
              (= :enum (:type er)))]}
  (assoc er :full-ns-name (conj ns (:name er))))

(defn namespace-enum-records
  "Adds full ns to all enums in a file record. Reads the :full-ns-name from the message record, returns the modified message record"
  [mr]
  {:pre [(and (map? mr)
              (contains? mr :full-ns-name))]}
  (let [ns (:full-ns-name mr)
        enums (get mr :enums [])]
    (assoc mr
      :enums (map #(namespace-enum-record % ns) enums))))

(declare namespace-message-record)

(defn namespace-message-records
  "adds full ns to all nesteds in a message record"
  [mr]
  {:pre [(and (map? mr)
              (contains? mr :full-ns-name))]}
  (let [ns (:full-ns-name mr)
        nesteds (get mr :nesteds [])]
    (assoc mr
      :nesteds (map #(namespace-message-record %1 ns) nesteds))))

(defn namespace-message-record
  "Adds a full namespace property to a message record. ns is a vector of strings, mr is the message record"
  [mr ns]
  {:pre [(and (vector? ns)
              (map? mr)
              (= :message (:type mr)))]}
  (let [new-ns (conj ns (:name mr))]
    (-> (assoc mr :full-ns-name new-ns)
        namespace-enum-records
        namespace-message-records)))

(defn extract-all-messages
  "extracts nested messages from a message record"
  [mr]
  (concat [(dissoc mr :nesteds)]
          (mapcat extract-all-messages (:nesteds mr))))

(defn extract-all-enums
  "extracts all enums from a message record and all nested message records. Assums that the message record already has a member called :full-ns-name"
  [mr]
  {:pre [(and (map? mr)
              (contains? mr :full-ns-name))]}
  (let [this-ns (:full-ns-name mr)
        this-enums (->> (:enums mr)
                        (map #(namespace-enum-record % this-ns)))]
  (concat this-enums
          (mapcat extract-all-enums (:nesteds mr)))))

(defn qualify-file-record
  "Qualifies every message definition in a proto file record with namespaces and options"
  [fr]
  (let [{package :package} fr
        options (extract-file-options fr)
        namespaced-msgs (->> (:contents fr)
                             (filter #(= :message (:type %)))
                             (map #(namespace-message-record % package)))
        msgs (->> namespaced-msgs
                  (mapcat extract-all-messages)
                  (map #(assoc % :options options))
                  (map #(dissoc % :enums)))
        msg-enums (->> namespaced-msgs
                       (mapcat extract-all-enums))
        enums (->> (:contents fr)
                   (filter #(= :enum (:type %)))
                   (map #(namespace-enum-record % package))
                   (concat msg-enums)
                   (map #(assoc % :options options)))
        msg-enums (->> msgs
                       (map namespace-enum-records))]
    (concat msgs enums)))

(defn create-namespace
  "Takes a list of definitions and makes a namespace (map) out of each item"
  [xs]
  (apply merge (map #(hash-map (:full-ns-name %) %) xs)))

(defn make-all-namespace-resolutions
  "makes up a bunch of names in order of likelyhood that a symbol might resolve to. Similar to c++ ns resolution. name-parts is the broken-down parts of the declared symbol, a vector. enclosing-ns is also a vector and the namespace parts of the type that encloses this reference."
  [enclosing-ns name-parts]
  (let [names (seq name-parts)
        ns (seq enclosing-ns)]
    (loop [res []
           rst ns]
      (if (empty? rst) (conj res (vec names))
          (recur (conj res (vec (concat rst names)))
                 (butlast rst))))))

(defn make-symbol-resolver
  "returns a function that can resolve a symbol against the namespace"
  [ns]
  (fn [enclosing-type-name symbol-name]
    (let [names (->> (make-all-namespace-resolutions enclosing-type-name
                                                     symbol-name)
                     (sort-by count))]
      (println (str names))
      (loop [test (first names)
             rst (rest names)]
        (if (nil? test)
          (throw (java.lang.Exception. "symbol cannot be found in namespace"))
          (if (contains? ns test)
            test
            (recur (first rst)
                   (rest rst))))))))

(defn resolve-references
  "resolve all symbol references in message-members in a namespace ns"
  [ns]
  (let [all-symbols (keys ns)]
    (letfn [(symbol-exists [s]
              (if (some? #(= % s) all-symbols)
                true false))
            (make-all-name-variations [member-type-name message-full-name]
              ;; gets all possible symbol names for a type
              ;; based on the enclosing message's fully qualified name
              ;; there are specific rules for how names are resolved
              (loop [res []
                     names message-full-name]
                (if (empty? names) res
                    (recur (conj res
              
            (resolve-member [x]
              (if (= true (:member-is-simple-type x))
                x
                (assoc x :resolved true)))
            (resolve-message [x]
              (assoc x
                :message-members
                (map resolve-member (:message-members x))))
            (resolve-x [x]
              (if (= :message (:type x))
                (resolve-message x)
                x))]
      (->> (map #(hash-map (first %) (-> % second resolve-x)) ns)
           (apply merge)))))
       
(defn lex
  "Runs the lexical analyzer on a stream of tokens, typically output from the parser. The proto token stream may contain import statements (similar to C-style include directives, which import definitions from other files. The file-tokenizer is a function that takes this file string and resolves it to a stream of tokens."
  [stream file-tokenizer]
  (let [result (->> (run (match-proto-file) stream)
                    (copy-meta stream))
        parts (group-by #(= :import (:type %)) (:contents result))
        import-fn #(lex (file-tokenizer (get % :value)) file-tokenizer)]
    (apply list
           (assoc result
             :contents
             (get parts false))
           (flatten (map import-fn (get parts true))))))
    
(defn parse-proto-file
  "Takes a filename and a function that turns filenames into streams of chars. Returns a seq of message records that each might contain message, enum definitions, declare packages, options etc." 

  [file-name file-resolver]
  (letfn [(import-fn [f] (-> (file-resolver f)
                             parser/parse))]
    (lex (import-fn file-name)
         #(import-fn %))))

(defn parse-and-process-file
  "parses a proto file, applies options to definitions, namespace all items and returns a flat list with all of the full qualified declarations and references"
  [file-name file-resolver]
  (let [ns (->> (parse-proto-file file-name file-resolver)
                (mapcat qualify-file-record)
                create-namespace
                resolve-references)]
    ns))

(defn resource-file-resolver
  "creates a file resolver that reads from the resource path"
  []
  (fn [f]
    (-> (clojure.java.io/resource f)
        (.getFile)
        slurp)))

(defn main
  "The lex app which shows a symbolic representation of the protobuf files"
  [& args]
  (let [[options extra banner] 
        (cli args
             ["-d" "--directory" "The directory in from which to resolve file names"
              :parse-fn parse-directory
              :default (java.io.File. ".")])]
    (println (str "Running from: " (.getAbsolutePath (:directory options))))))
  
          
          
