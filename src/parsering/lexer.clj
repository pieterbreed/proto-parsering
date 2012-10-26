(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

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
            :value package})))

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
                        (let->> [s (match-symbol)]
                                (always {:member-type (:value s)
                                         :member-is-simple-type false})))
           name (match-symbol)
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
                   :name (:value name)
                   :option option
                   :tag (:value position)})))

; forward declaration so that messages may have other messages in them
(def match-message)

(defparser match-message-item []
  (choice (match-message)
          (match-message-member)
          (match-enum)))                             

(defparser match-message []
  (let->> [_ (match-keywords :message)
           name (match-symbol)
           _ (match-keywords :open-curly)
           items (many (match-message-item))
           _ (match-keywords :close-curly)]
          (always {:type :message
                   :items items})))

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

(defn lex
  "Runs the lexical analyzer on a stream of tokens, typically output from the parser. The proto token stream may contain import statements (similar to C-style include directives, which import definitions from other files. The file-tokenizer is a function that takes this file string and resolves it to a stream of tokens."
  [stream file-tokenizer]
  (let [meta-data (get (meta stream) :parsering-data {})
        result (-> (run (match-proto-file) stream)
                   (with-meta meta-data))
        imports (filter #(= :import (:type %)) (:contents result))
        import-fn #(lex (file-tokenizer (get % :value)) file-tokenizer)]
    (apply list result (flatten (map import-fn imports)))))
    


(defn parse-proto-file
  "Takes a filename and a function that turns filenames into tokenized streams (using parser/parse). If the resolver brings back meta-data with the value of :parsering-data it will be copied to the result stream"

  [file file-resolver]
  (letfn [(import-fn [f] (-> (file-resolver f)
                             parsering.parser/parse))]
    (lex (import-fn file)
         #(import-fn %))))
           
          
          
