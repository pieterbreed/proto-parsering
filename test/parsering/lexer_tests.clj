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
              :values [{:name "Visible"
                        :value 1}
                       {:name "Hidden"
                        :value 2}
                       {:name "Something"
                        :value 3}]})))))

(deftest message
  (testing "one ugly difficult to debug integrated message"
    (are [input t]
        (let [msg (the.parsatron/run (match-message)
                                     (parsering.parser/parse input))]
          (t msg))

        "message Test { required int32 s = 1; }"
        #(and (= :message (:type %))
              (= [] (:nesteds %))
              (= :message-member (get-in % [:members 0 :type]))
              (= :required (get-in % [:members 0 :modifier]))
              (= :int32 (get-in % [:members 0 :member-type]))
              (= "s" (get-in % [:members 0 :name]))
              (= 1 (get-in % [:members 0 :tag])))


        "message outer { message inner { required double first = 1; repeated float second = 2; } optional bool third = 3; }"
        #(and (= :message (get-in % [:nesteds 0 :type]))
              (= :repeated (get-in % [:nesteds 0 :members 1 :modifier]))
              (= "first" (get-in % [:nesteds 0 :members 0 :name]))
              (= 2 (get-in % [:nesteds 0 :members 1 :tag]))
              (= :optional (get-in % [:members 0 :modifier])))
        


        )))


                                
                              

(run-all-tests #"parsering.lexer-test")
