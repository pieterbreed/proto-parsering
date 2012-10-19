(ns parsering.parser
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defn parse-int [str]
  (Integer/parseInt str))

(def whitespace-char (token #{\space \newline \tab \return}))
(defparser whitespace [] (many whitespace-char) (always {:type :whitespace}))
(defparser whitespace1 [] (many1 whitespace-char) (always {:type :whitespace}))

(defn -make-char-sequence-test [chars result]
  (list
   `attempt
   (conj
    (concat (map #(list `the.parsatron/char %) chars)
            (list (list `always result)))
    `>>)))

(defmacro -string-char-options [& args]
  (let [pairs (partition 2 args)
        choices (map #(-make-char-sequence-test (first %) (second %)) pairs)]
    (conj choices `choice)))


(defparser string-char []
  (choice (-string-char-options [\\ \\] \\
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
                            :value-type :string
                            :value (apply str strvalue)}))))

(defparser int-value []
  (let->> [v (many1 (digit))]
          (always {:type :value
                   :value (parse-int (apply str v))
                   :value-type :int})))


(defparser symbol-value []
  (let->> [frst (letter)
           rst (many (choice (letter)
                             (digit)
                             (char \_)))]
          (always {:type :symbol
                   :value (str frst (apply str rst))})))

(defn -flags-item [flag-str flag-symbol-str]
  (list `attempt
        (list `>>
              `(string ~flag-str)
              `(always {:type :keyword
                        :value ~(keyword flag-symbol-str)}))))

(defmacro flags [& flags]
  (let [items (map #(-flags-item % %) flags)]
    `(choice ~@items)))

(defmacro non-standard-flags [& flags]
  (let [pairs (partition 2 flags)
        items (map #(apply -flags-item %) pairs)]
    `(choice ~@items)))
    

(defparser parser []
  (let->> [parsed (many (choice
                         (whitespace1)
                         (flags "message"
                                "required" "optional" "repeated"
                                "double" "float"
                                "int32" "int64"
                                "uint32" "uint64"
                                "sint32" "sint64"
                                "fixed32" "fixed64"
                                "sfixed32" "sfixed64"
                                "bool"
                                "string"
                                "bytes"
                                "enum"
                                "import"
                                "package"
                                "extend")
                         (non-standard-flags ";" "semicolon"
                                             "}" "close-curly"
                                             "{" "open-curly"
                                             "=" "equals")
                         (int-value)
                         (string-value)
                         (symbol-value)))
           rest (>> (many (any-char)))]

          (if (< 0 (count rest))
             (do
               (println (str "unknown: " (apply str (take 10 rest))))
               (never))
             (->> parsed
                  (filter #(not (= :whitespace (:type %))))
                  always))))
                         
          ;;   (always (-> parsed
          ;;               (filter #(not (= :whitespace (:type %)))))))))
          

                       