(ns parsering.lexer-test
  (:use clojure.test
        parsering.lexer))


(defn parse [str]
  (list (the.parsatron/run (parsering.parser/parse-single) str)))

(deftest test-bools
  (testing "that symbols that look like booleans get turned into boolean values"
    (are [in val] (= {:type :value
                      :value-type :bool
                      :value val}
                     (the.parsatron/run (bool-value) (parse in)))
         "true" true)))


(run-all-tests #"parsering.lexer-test")
