(ns parsering.lexer-test
  (:use clojure.test
        parsering.lexer))

(deftest package
  (testing "that the package declaration works even when it's not there"
    (are [input package] (let [pac (the.parsatron/run (match-package) (parsering.parser/parse input))]
                           (and (= :package (:type pac))
                                (= package (:value pac))))

         "package one.two.three;" "one.two.three"

         "" "")))

(deftest single-option
  (testing "that options can be parsed"
    (are [input sym val typ] (let [opt (the.parsatron/run (match-option)
                                                          (parsering.parser/parse input))]
                               (and (= :option (:type opt))
                                    (= sym (:key opt))
                                    (= val (:value opt))
                                    (= typ (:value-type opt))))

         "option symbol = 1;" "symbol" 1 :int

         "option ss = \"oeu\";" "ss" "oeu" :string

         "option oeu.oeu.oeu = eui.eui;" "oeu.oeu.oeu" "eui.eui" :symbol
         )))

(deftest single-import
  (testing "that import statements are parsed"
    (are [input file] (let [imp (the.parsatron/run (match-import)
                                                   (parsering.parser/parse input))]
                        (and (= (:import (:type imp)))
                             (= file (:value imp))))

         "import \"import.proto\";" "import.proto")))

(deftest enum-test
  (testing "that an enum can have members with values"
    (let [input "enum Scope { Visible = 1; Hidden = 2; Something = 3; } "
          en (the.parsatron/run (match-enum)
                                (parsering.parser/parse input))]
      (is (= en
             {:type :enum
              :name "Scope"
              :values [{:name "Visible", :value 1}
                       {:name "Hidden", :value 2}
                       {:name "Something", :value 3}]})))))

(deftest message
  (testing "one ugly difficult to debug integrated message"
    (are [input expected]
        (let [msg (the.parsatron/run (match-message)
                                     (parsering.parser/parse input))]
          (= expected msg))

        "message Test { required int32 s = 1; }"
        {:nesteds []
         :enums []
         :type :message
         :name "Test"
         :message-members [{:type :message-member
                            :modifier :required
                            :member-type :int32
                            :member-is-simple-type true
                            :name "s"
                            :option nil
                            :tag 1}]}

        "message outer { message inner { required double first = 1; repeated float second = 2; } optional bool third = 3; }"
        {:nesteds [{:nesteds [], :enums [], :message-members [{:type :message-member, :modifier :required, :member-type :double, :member-is-simple-type true, :name "first", :option nil, :tag 1} {:type :message-member, :modifier :repeated, :member-type :float, :member-is-simple-type true, :name "second", :option nil, :tag 2}], :type :message, :name "inner"}], :enums [], :message-members [{:type :message-member, :modifier :optional, :member-type :bool, :member-is-simple-type true, :name "third", :option nil, :tag 3}], :type :message, :name "outer"})))


                                
                              

(run-all-tests #"parsering.lexer-test")
