(ns parsering.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron])
  (:gen-class))

;; (defn parse-int [str]
;;   (Integer/parseInt str))

;; (def whitespace-char (token #{\space \newline \tab \return}))

;; (defparser whitespace [] (many whitespace-char) (always {:type :whitespace}))
;; (defparser whitespace1 [] (many1 whitespace-char) (always {:type :whitespace}))

;; ;; (defparser bool-value []
;; ;;   (either (>> (choice (string "true")
;; ;;                       (string "True")
;; ;;                       (string "1"))
;; ;;               (always {:type :value
;; ;;                        :value {:type :bool
;; ;;                                :value true}}))
;; ;;           (>> (choice (string "false")
;; ;;                       (string "False")
;; ;;                       (string "0"))
;; ;;               (always {:type :value
;; ;;                        :value {:type :bool
;; ;;                                :value false}}))))

;; (defn -make-char-sequence-test [chars result]
;;   (list
;;    `attempt
;;    (conj
;;     (concat (map #(list `the.parsatron/char %) chars)
;;             (list (list `always result)))
;;     `>>)))

;; (defmacro -string-char-options [& args]
;;   (let [pairs (partition 2 args)
;;         choices (map #(make-char-sequence-test (first %) (second %)) pairs)]
;;     (conj choices `choice)))


;; (defparser string-char []
;;   (choice (-string-char-options [\\ \\] \\
;;                                 [\\ \n] \newline
;;                                 [\\ \t] \tab
;;                                 [\\ \r] \return
;;                                 [\\ \"] \")
;;           (except-char "\"")))

;; (defparser string-value []
;;   (between (char \")
;;            (char \")
;;            (let->> [strvalue (many (string-char))]
;;                    (always {:type :value
;;                             :value {:type :string
;;                                     :value (apply str strvalue)}}))))


;; (defparser namespace-string []
;;   (let->> [firstpart (many1 (letter))
;;            other (many (let->> [_ (char \.)
;;                                 wordpart (many1 (letter))]
;;                                (always (apply str wordpart))))]
;;           (always (-> (apply str firstpart)
;;                       vector
;;                       (concat other)
;;                       vec))))
                 

;; (defparser package-declaration []
;;   (whitespace)
;;   (let->> [ns-vector (either (between (>> (string "package") (whitespace1))
;;                                       (char \;)
;;                                       (namespace-string))
;;                              (always []))]
;;           (always {:type :namespace
;;                    :namespace ns-vector})))

;; (defparser symbol-value []
;;   (let->> [frst (letter)
;;            rst (many (choice (letter)
;;                              (digit)
;;                              (char \_)))]
;;           (always {:type :value
;;                    :value {:type :symbol
;;                            :value (str frst (apply str rst))}})))

;; (defparser int-value []
;;   (let->> [v (many1 (digit))]
;;           (always {:type :value
;;                    :value {:type :int
;;                            :value (parse-int (apply str v))}})))

;; (defparser value-value []
;;   (choice (symbol-value)
;;           (int-value)
;;           (string-value)))

;;  (defparser option-parser []
;;    (let->> [_ (>> (whitespace) (string "option"))
;;             option-name (>> (whitespace) (symbol-value))
;;             _ (>> (whitespace) (char \=))
;;             value (>> (whitespace) (value-value))
;;             _ (>> (whitespace) (char \;))]
;;            (always {:type :option
;;                     :value {:name option-name
;;                             :value value}})))

;; (defn -flags-item [flag-str]
;;   (list `attempt
;;         (list `>>
;;               `(string ~flag-str)
;;               `(always ~(keyword flag-str)))))

;; (defmacro flags [& flags]
;;   (let [items (map -flags-item flags)]
;;     `(choice ~@items)))
  

;; (defparser msg-line []
;;   (let->> [modifier (>> (whitespace)
;;                         (flags "required" "optional" "repeated"))
;;            value-type (>> (whitespace1)
;;                           (flags "double" "float"
;;                                  "int32" "int64"
;;                                  "uint32" "uint64"
;;                                  "sint32" "sint64"
;;                                  "fixed32" "fixed64"
;;                                  "sfixed32" "sfixed64"
;;                                  "bool"
;;                                  "string"
;;                                  "bytes"))
;;            symbol (>> (whitespace1)
;;                       (symbol-value))
;;            _ (>> (whitespace1)
;;                  (char \=))
;;            position (>> (whitespace1)
;;                         (int-value))]
;;           (always {:type :msg-line
;;                    :value {:modifier modifier
;;                            :value-type value-type
;;                            :symbol symbol
;;                            :position position}})))
  

;; (defparser message []
;;   (let->> [_ (>> (whitespace) (string "message"))
;;            msg-name-symbol (>> (whitespace) (symbol-value))
;;            _ (>> (whitespace) (char \{))
;;            _ (>> (whitespace) (char \}))]
;;           (always {:type :message
;;                    :value {:name msg-name-symbol}})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
