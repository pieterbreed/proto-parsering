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

;; (deftest bool-test
;;   (testing "bool values are parsed"
;;     (are [input] (= {:type :value
;;                      :value {:type :bool
;;                              :value true}} (the.parsatron/run (bool-value) input))
;;          "true"
;;          "True"`
;;          "1")
;;     (are [input] (= {:type :value
;;                      :value {:type :bool
;;                              :value false}} (the.parsatron/run (bool-value) input))
;;          "false"
;;          "False"
;;          "0")))

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
    (is (= {:type :value
            :value {:type :string
                    :value "testing"}}
           (the.parsatron/run (string-value) "\"testing\"")))))

(deftest symbols-test
  (testing "whether symbols may start with letters, then have many letters and digits and underscores in them"
    (are [expected input] (= {:type :value
                              :value {:type :symbol
                                      :value expected}}
                             (the.parsatron/run (symbol-value) input))
         "aoeu" "aoeu"
         "oeu1" "oeu1"
         "oeu_oeu" "oeu_oeu")))

(deftest symbols-not-test
  (testing "that symbols starting with digits or underscores are not valid"
    (are [input] (thrown? RuntimeException (the.parsatron/run (symbol-value) input))
         "_aoeu"
         "2oeu")))

(deftest value-value-test
  (testing "that where values are expected, all the types can be parsed"
    (are [input type value] (= {:type :value
                                :value {:type type
                                        :value value}}
                               (the.parsatron/run (value-value) input))
         "true" :symbol "true"
         "False" :symbol "False"
         "\"testing\"" :string "testing"
         "symbol" :symbol "symbol")))

(deftest option-parser-test
  (testing "that options can be parsed"
    (are [input
          symbol-name
          symbol-type
          symbol-value] (= {:type :option
                                        :value
                                        {:name {:type :value
                                                :value {:type :symbol
                                                        :value symbol-name}}
                                         :value {:type :value
                                                 :value {:type symbol-type
                                                         :value symbol-value}}}}
                                       (the.parsatron/run (option-parser) input))
         "option java_package = \"com.allangray.model\";"
         "java_package"
         :string
         "com.allangray.model"

         "option java_multiple_files = true;"
         "java_multiple_files"
         :symbol
         "true")))

(deftest msg-line-test
  (testing "that message item lines can be parsed"
    (are [input
          modifier]
         (= {:type :msg-line
             :value {:modifier modifier}}
            (the.parsatron/run (msg-line) input))

         "required" :required

         " required" :required
         "optional" :optional
         "\t\trepeated" :repeated)))
         

(run-all-tests #"parsering.core-test")