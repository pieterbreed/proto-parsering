(ns proto.decoder2-tests
  (:refer-clojure :exclude [char])
  (:use [clojure.test]
        [proto.decoder2]
        [the.parsatron]
        [debug.debug]))

(defmacro defunzip-val-test
  "tests an unzipping operation that results in a single value"
  [name testing operation & rest]
  `(deftest ~name
     (testing ~testing
       (are [seq#] (let [[res# bs#] seq#]
                    (= res# (run (~operation) bs#)))
            ~@rest))))

(defmacro defunzip-vec-test
  "tests an unzipping operation that results in a single value"
  [name testing operation & rest]
  `(deftest ~name
     (testing ~testing
       (are [seq#] (let [[res# bs#] seq#]
                    (= res# (vec (run (~operation) bs#))))
            ~@rest))))



(defunzip-val-test varint-from-stream-works2
 "happy cases: varints"
 varint
 [1 [1]]
 [300 [172 2]]
 [-753 [0x8f,0xfa,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1]])

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
  [300 [44 1 0 0 0 0 0 0]]
  [-4565996203501588736 [0x0,0x6f,0x81,0x4,0xa5,0x52,0xa2,0xc0]])

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

(with-debugging (run-all-tests #"proto.decoder2-tests"))