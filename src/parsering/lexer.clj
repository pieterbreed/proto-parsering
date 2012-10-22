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
              pac (match-symbol)]
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
                                         :value (:value enumvalue)})))]
          (always {:type :enum
                   :values (vec enums)})))

(defparser match-message-member []
  (let->> [modifier (choice (match-keywords :optional)
                            (match-keywords :required)
                            (match-keywords :repeated))
           type (match-keywords :double :float
                                :int32 :int64
                                :uint32 :uint64
                                :sint32 :sint64
                                :fixed32 :fixed64
                                :sfixed32 :sfixed64
                                :bool
                                :string
                                :bytes
                                :enum)
           name (match-symbol)
           _ (match-keywords :equals)
           position (match-int-value)
           _ (match-keywords :semicolon)]
          (always {:type :message-member
                   :modifier (:value modifier)
                   :member-type (:value type)
                   :name (:value name)
                   :tag (:value position)})))
                                                     

(defparser match-message []
  (let->> [_ (match-keywords :message)
           name (match-symbol)
           _ (match-keywords :open-curly)
           nesteds (many (match-message))
           members (many (match-message-member))
           _ (match-keywords :close-curly)]
          (always {:type :message
                   :nesteds (vec nesteds)
                   :members (vec members)})))
           
          
          
