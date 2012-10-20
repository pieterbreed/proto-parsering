(ns parsering.lexer-test
  (:use clojure.test
        parsering.lexer))

(deftest package
  (testing "that the package declaration works even when it's not there"
    (are [input package] (= :

(run-all-tests #"parsering.lexer-test")
