(ns parsering.lexer
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser match-keyword [keyword]
  (token #(and (= :keyword (:type %))
               (= :package (:value %)))))

(defparser match-symbol []
  (token #(and (= :symbol (:type %)))))

(defparser match-package []
  (>> (match-keyword :package)))

          ;;  fst (match-symbol)]
          ;; fst))
           ;;  rst (many (>> (match-keyword :point)
          ;;                (match-symbol)))]
          ;; (conj rst fst)))

                       
              
             
                     
