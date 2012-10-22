(ns parsering.parser-test
  (:use clojure.test
        parsering.parser))

(deftest whitespace-test
  (testing "all whitespaces are handled"
    (are [input] (= {:type :whitespace} (the.parsatron/run (whitespace) input))
         " "
         "\t"
         "\n"
         "\r"
         "\n\r"
         "    "
         "\t  \n")))

(deftest string-char-test
  (testing "that characters in strings are escaped and that escaped quotes don't terminate the string"
    (are [expected input] (= expected (the.parsatron/run (string-char) input))
         \tab "\t"
         \newline "\n"
         \return "\r"
         \\ "\\\\"
         \" "\\\""
         \a "a"
         \b "b")))

(deftest string-test
  (testing "whether string values can be parsed"
    (are [expected input]
         (= {:type :value
            :value-type :string
            :value expected}
            (the.parsatron/run (string-value)
                               input))

         "testing" "\"testing\""
         "oeu\"oeu" "\"oeu\\\"oeu\"")))

(deftest symbols-test
  (testing "whether symbols may start with letters, then have many letters and digits and underscores in them"
    (are [expected input] (= {:type :symbol
                              :value expected}
                             (the.parsatron/run
                              (symbol-value)
                              input))
         "aoeu" "aoeu"
         "oeu1" "oeu1"
         "oeu_oeu" "oeu_oeu")))

(deftest symbols-not-test
  (testing "that symbols starting with digits or underscores are not valid"
    (are [input] (thrown? RuntimeException (the.parsatron/run (symbol-value) input))
         "_aoeu"
         "2oeu")))


(deftest int-value-test
  (testing "that an integer value parses correctly"
    (are [input value]
         (= {:type :value
             :value value
             :value-type :int}
            (the.parsatron/run (int-value) input))
         "43" 43)))

(deftest comments-test
  (testing "that comments work"
    (are [input expected]
         (= expected
            (parsering.parser/parse input))

         "option o; // this is the comment\n message Hey"
         (list
          {:type :keyword, :value :option}
          {:type :symbol, :value "o"}
          {:type :keyword, :value :semicolon}
          {:type :keyword, :value :message}
          {:type :symbol, :value "Hey"}))))
           

(run-all-tests #"parsering.parser-test")