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
   [9]])

(defunzip-val-test item-works
  "that item unzips a tag with a value properly"
  item
  [{:wire-type :varint
    :tag-nr 1
    :value 150} [0x08 0x96 0x01]])

;; Kom ons bou 'n 'SimpleTypesMsg'!
;; signed int: -12
;; double :32.23
;; small int :22
;; string :this is a string
;; signed big int: -33344455566

;; Result: [ 8 -12 -1 -1 -1 -1 -1 -1 -1 -1 1 16 44 25 61 10 -41 -93 112 29 64 64 34 16 116 104 105 115 32 105 115 32 97 32 115 116 114 105 110 103 40 -101 -114 -32 -73 -8 1] 

(deftest real-simple-message-decode-test
  (testing "we're deserializing a real message here encoded using google protobufs encoder"
    


(run-all-tests #"proto.decoder2-tests")