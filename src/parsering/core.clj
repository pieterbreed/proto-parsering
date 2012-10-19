(ns parsering.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron])
  (:gen-class))

(def whitespace-char (token #{\space \newline \tab \return}))

(defparser whitespace [] (many whitespace-char) (always {:type :whitespace}))
(defparser whitespace1 [] (many1 whitespace-char) (always {:type :whitespace}))

(defparser bool-value []
  (either (>> (choice (string "true")
                      (string "True")
                      (string "1"))
              (always {:type :value
                       :value {:type :bool
                               :value true}}))
          (>> (choice (string "false")
                      (string "False")
                      (string "0"))
              (always {:type :value
                       :value {:type :bool
                               :value false}}))))

(defn -make-char-sequence-test [chars result]
  (list
   `attempt
   (conj
    (concat (map #(list `the.parsatron/char %) chars)
            (list (list `always result)))
    `>>)))

(defmacro string-char-options [& args]
  (let [pairs (partition 2 args)
        choices (map #(make-char-sequence-test (first %) (second %)) pairs)]
    (conj choices `choice)))


(defparser string-char []
  (choice (string-char-options [\\ \\] \\
                               [\\ \n] \newline
                               [\\ \t] \tab
                               [\\ \r] \return
                               [\\ \"] \")
          (except-char "\"")))

(defparser string-value []
  (between (char \")
           (char \")
           (let->> [strvalue (many (string-char))]
                   (always {:type :value
                            :value {:type :string
                                    :value (apply str strvalue)}}))))


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
