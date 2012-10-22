(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser match-keyword [keyword]
  (token #(and (= :keyword (:type %))
               (= keyword (:value %)))))

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
     (let->> [_ (match-keyword :package)
              fst (match-symbol)
              rst (many (>> (match-keyword :point)
                            (match-symbol)))]
             (->> (apply list fst rst)
                  (map :value)
                  (apply vector)
                  always))
     (always []))]
   (always {:type :package
            :value package})))

(defparser match-option []
  (let->> [_ (match-keyword :option)
           option-name (match-symbol)
           _ (match-keyword :equals)
           option-value (either (let->> [s (match-symbol)]
                                        (always {:type :symbol
                                                 :value (:value s)}))
                                (let->> [s (match-string-value)]
                                        (always {:type :string
                                                 :value (:value s)})))
           _ (match-keyword :semicolon)]
          (always {:type :option
                   :key (:value option-name)
                   :value-type (:type option-value)
                   :value (:value option-value)})))
          
(defparser match-import []
  (let->> [_ (match-keyword :import)
           location (match-string-value)
           _ (match-keyword :semicolon)]

          (always {:type :import
                   :value (:value location)})))

(defparser match-message-member []
  (let->> [modifier (choice (match-keyword :optional)
                            (match-keyword :required)
                            (match-keyword :repeated))
           type (match-symbol)
           name (match-symbol)
           _ (match-keyword :equals)
           position (match-int-value)]
          (always {:type :message-member
                   :modifier (:type modifier)
                   :member-type (:value type)
                   :name (:value name)
                   :position (:value position)})))
                                                     

;; (defparser match-message []
;;   (let->> [_ (match-keyword :message)
;;            name (match-symbol)
;;            _ (match-keyword :open-curly)
;;            nesteds (many (match-message))
           
          
          
