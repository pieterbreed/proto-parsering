(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser match-keyword [keyword]
  (token #(and (= :keyword (:type %))
               (= keyword (:value %)))))

(defparser match-symbol []
  (token #(and (= :symbol (:type %)))))

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

(defparser match-options []
  (let->> [options (many
                    (let->> [_ (match-keyword :option)
                             option-name (match-symbol)
                             _ (match-keyword :equals)
                             option-value (match-symbol)
                             _ (match-keyword :semicolon)]

                            (always
                             {:name (:value option-name)
                              :value (:value option-value)})))]
          (always {:type :options
                   :value options})))
          
          
