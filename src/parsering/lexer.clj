(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser match-keyword [keyword]
  (token #(and (= :keyword (:type %))
               (= keyword (:value %)))))

(defparser match-symbol []
  (token #(and (= :symbol (:type %)))))

(defparser match-package []
  (let->> [_ (match-keyword :package)
           fst (match-symbol)
           rst (many (>> (match-keyword :point)
                         (match-symbol)))]
          (let [package-items (apply list fst rst)]
            (->> package-items
                 (map :value)
                 (apply vector)
                 ((fn [v]
                   {:type :package
                    :value v}))
                 always))))


                       
              
             
                     
