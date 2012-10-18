(ns parsering.core-test
  (:use clojure.test
        parsering.core))

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

(deftest bool-test
  (testing "bool values are parsed"
    (are [input] (= {:type :value
                     :value {:type :bool
                             :value true}} (the.parsatron/run (bool) input))
         "true"
         "True"
         "1")
    (are [input] (= {:type :value
                     :value {:type :bool
                             :value false}} (the.parsatron/run (bool) input))
         "false"
         "False"
         "0")))

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

         
         

(run-all-tests #"parsering.core-test")