(ns parsering.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron])
  (:gen-class))

(def whitespace-char (token #{\space \newline \tab \return}))
(def not-close-string (taken #(not=

(defparser whitespace [] (many whitespace-char) (always {:type :whitespace}))
(defparser whitespace1 [] (many1 whitespace-char) (always {:type :whitespace}))

(defparser bool []
  (either (>> (choice (string "true")
                      (string "True")
                      (string "1"))
              (always {:value {:type :bool
                               :value true}}))
          (>> (choice (string "false")
                      (string "False")
                      (string "0"))
              (always {:type :value
                       :value {:type :bool
                               :value false}}))))

(defparser string []
  (let->> (between (char \")
                   (char \")
                   (
  

(defparser namespace-string []
  (let->> [firstpart (many1 (letter))
           other (many (let->> [_ (char \.)
                                wordpart (many1 (letter))]
                               (always (apply str wordpart))))]
          (always (-> (apply str firstpart)
                      vector
                      (concat other)
                      vec))))
                 

(defparser package-declaration []
  (whitespace)
  (let->> [ns-vector (either (between (>> (string "package") (whitespace1))
                                      (char \;)
                                      (namespace-string))
                             (always []))]
          (always {:type :namespace
                   :namespace ns-vector})))

;; (defparser option-parser []
;;   (whitespace)
  
  
           
  
  
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
