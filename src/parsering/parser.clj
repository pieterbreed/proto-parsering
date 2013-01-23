(ns parsering.parser
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]
        [parsering.common]))

(defn except-char
  "Consume any characters except the ones provided"
  [exclusions]
  (token (fn [inp]
           (and (char? inp)
                (not (some #(= % inp) exclusions))))))

(defn parse-int [str]
  (Integer/parseInt str))

(def whitespace-char (token #{\space \newline \tab \return}))
(defparser whitespace [] (many whitespace-char) (always {:type :whitespace}))
(defparser whitespace1 [] (many1 whitespace-char) (always {:type :whitespace}))

(defparser parse-comment []
  (attempt
   (let->> [_ (char \/)
            _ (char \/)
            msg (many (except-char "\n"))]
           (always {:type :comment
                    :value (apply str msg)}))))

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
  (let->> [sign (choice (>> (char \-)
                            (always :minus))
                        (>> (char \+)
                            (always :plus))
                        (always :plus))
           v (many1 (digit))]
          (always {:type :value
                   :value (if (= :minus sign)
                            (* -1 (parse-int (apply str v)))
                            (parse-int (apply str v)))
                   :value-type :int})))

(defparser symbol-value-word []
  (let->> [frst (letter)
           rst (many (choice (letter)
                             (digit)
                             (char \_)))]
          (always (str frst (apply str rst)))))

(defparser symbol-full-value []
  (let->> [frst (symbol-value-word)
           rst (many (attempt (>> (char \.)
                                  (symbol-value-word))))]
          (always {:type :symbol
                   :fully-qualified false
                   :value (->> rst
                               (apply conj [] frst)
                               (clojure.string/join "."))})))

(defparser symbol-value []
  (choice (let->> [_ (char \.)
                   v (symbol-full-value)]
                  (always assoc v :fully-qualified true))
          (let->> [s (symbol-full-value)]
                  (always s))))

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

(defparser parse-single []
  (choice
   (parse-comment)
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
          "option"
          "extend")
   (non-standard-flags ";" "semicolon"
                       "." "point"
                       "}" "close-curly"
                       "{" "open-curly"
                       "[" "open-bracket"
                       "]" "close-bracket"
                       "=" "equals")
   (int-value)
   (string-value)
   (symbol-value)))


(defparser parser []
  (let->> [parsed (many (parse-single))
           rest (>> (many (any-char)))]
          (if (< 0 (count rest))
            (never)
            (always parsed))))

(defn whitespace? [item]
  (= :whitespace (:type item)))

(defn comment? [item]
  (= :comment (:type item)))

(defn make-symbol-test []
  #(= :symbol (:type %)))

(defparser bool-value []
  (attempt
   (let->> [t (token (make-symbol-test))]
           (if (some #(= (:value t) %)
                     '("True" "true"))
             (always {:type :value
                      :value-type :bool
                      :value true})
             (if (some #(= (:value t) %)
                       '("False" "false"))
               (always {:type :value
                        :value-type :bool
                        :value false})
               (never))))))

(defparser cleaner []
  (many (choice (bool-value)
                (token (constantly true)))))


(defn parse
  "takes a string and transforms it into a stream of tokens. If the str has meta-data with key :parsering-data then it will be copied to the output stream"
  [str]
  (let [parsed (run (parser) str)]
  (->> (copy-meta str parsed)
       (filter #(not (whitespace? %)))
       (filter #(not (comment? %))))))
          

                       