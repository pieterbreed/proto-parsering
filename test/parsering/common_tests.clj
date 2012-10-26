(ns parsering.common-tests
  (:use clojure.test
        parsering.common))

(deftest meta-data-copy-test
  (testing "that meta data copies properly"
    (are [meta-source meta-dest meta-expected]
         (let [result (copy-meta meta-source meta-dest)]
           (and (= meta-dest result)
                (= meta-expected
                   (meta result))))

         (with-meta {:src-a :src-a} {:m-src-a "a"
                                     :parsering-data {:b :b
                                                      :c :c}})
         (with-meta {:dest-y :dest-y} {:m-dest-z :z})

         {:m-dest-z :z
          :parsering-data {:b :b
                           :c :c}})))
                                       



(run-all-tests #"parsering.common-tests")
