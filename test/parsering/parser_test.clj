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

;; (deftest value-value-test
;;   (testing "that where values are expected, all the types can be parsed"
;;     (are [input type value] (= {:type :value
;;                                 :value {:type type
;;                                         :value value}}
;;                                (the.parsatron/run (value-value) input))
;;          "true" :symbol "true"
;;          "False" :symbol "False"
;;          "\"testing\"" :string "testing"
;;          "symbol" :symbol "symbol")))

;; (deftest option-parser-test
;;   (testing "that options can be parsed"
;;     (are [input
;;           symbol-name
;;           symbol-type
;;           symbol-value] (= {:type :option
;;                                         :value
;;                                         {:name {:type :value
;;                                                 :value {:type :symbol
;;                                                         :value symbol-name}}
;;                                          :value {:type :value
;;                                                  :value {:type symbol-type
;;                                                          :value symbol-value}}}}
;;                                        (the.parsatron/run (option-parser) input))
;;          "option java_package = \"com.allangray.model\";"
;;          "java_package"
;;          :string
;;          "com.allangray.model"

;;          "option java_multiple_files = true;"
;;          "java_multiple_files"
;;          :symbol
;;          "true")))

;; (deftest msg-line-test
;;   (testing "that message item lines can be parsed"
;;     true))
;;     ;; (are [input
;;     ;;       modifier
;;     ;;       type]
;;     ;;      (= {:type :msg-line
;;     ;;          :value {:modifier modifier
;;     ;;                  :value-type type}}
;;     ;;         (the.parsatron/run (msg-line) input))

;;     ;;      "required int32" :required :int32

;;     ;;      " required double" :required :double
         
;;     ;;      "optional fixed64" :optional :fixed64
;;     ;;      "\t\trepeated\tbytes" :repeated :bytes)))

;; (deftest int-value-test
;;   (testing "that an integer value parses correctly"
;;     (are [input value]
;;          (= {:type :value
;;              :value {:type :int
;;                      :value value}}
;;             (the.parsatron/run (int-value) input))
;;          "43" 43)))

;; (deftest item-line-test
;;   (testing "that messages have lines and that those lines look sane"
;;     (are [input
;;           modifier
;;           value-type
;;           symbol-type
;;           position]
;;          (= {:type :msg-line
;;              :value {:modifier modifier
;;                      :value-type value-type
;;                      :symbol {:type :value :value {:type :symbol :value symbol-type}}
;;                      :position {:type :value :value {:type :int :value position}}}}
;;             (the.parsatron/run (msg-line) input))

;;          "optional string query = 1"
;;          :optional
;;          :string
;;          "query"
;;          1

;;          "required double items  = 2"
;;          :required
;;          :double
;;          "items"
;;          2

;;          "repeated int32 query = 101"
;;          :repeated
;;          :int32
;;          "query"
;;          101

;;          )))

(run-all-tests #"parsering.parser-test")