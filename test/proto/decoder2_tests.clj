(ns proto.decoder2-tests
  (:refer-clojure :exclude [char])
  (:use [clojure.test]
        [proto.decoder2]
        [the.parsatron]))

(defn make-bytes
  "maps uncheked-byte on the input seq"
  [xs]
  (map unchecked-byte xs))

(defmacro defunzip-val-test
  "tests an unzipping operation that results in a single value"
  [name testing operation & rest]
  `(deftest ~name
     (testing ~testing
       (are [seq#] (let [[res# bs#] seq#]
                    (= res# (run (~operation) (make-bytes bs#))))
            ~@rest))))

(defmacro defunzip-vec-test
  "tests an unzipping operation that results in a single value"
  [name testing operation & rest]
  `(deftest ~name
     (testing ~testing
       (are [seq#] (let [[res# bs#] seq#]
                    (= res# (vec (run (~operation) (make-bytes bs#)))))
            ~@rest))))



(defunzip-val-test varint-from-stream-works2
 "happy cases: varints"
 varint
 [1 [1]]
 [300 [172 2]])

(defunzip-vec-test
  length-delimited-works2
  "a few happy cases: length-delimited"
  length-delimited 
  [[1] [1 1]]
  [[1 2] [2 1 2]])

(defunzip-val-test sixty-four-byte-works
  "that constant-size 64-bit values works"
  fixed-64
  [1 [1 0 0 0 0 0 0 0]]
  [300 [44 1 0 0 0 0 0 0]])

(defunzip-val-test thirty-two-byte-works
  "that constant-size 32-bit values works"
  fixed-32
  [1 [1 0 0 0]]
  [300 [44 1 0 0]])

(defunzip-val-test tag-works
  "that tags can be decoded properly"
  tag
  [{:wire-type :varint
    :tag-nr 1}
   [8]]
  [{:wire-type :64-bit
    :tag-nr 1}
   [9]]
  [{:wire-type :32-bit
    :tag-nr 300}
   [144 101]])

(run-all-tests #"proto.decoder2-tests")