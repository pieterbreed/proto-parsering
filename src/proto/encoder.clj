(ns proto.encoder
  (:use [proto.common])
  (:require [clojure.math.numeric-tower :as math]))

(defmulti to-zigzag
  "performs zigzag encoding on a integral value"
  type)

(defmethod to-zigzag
  java.lang.Long [l]
  (bit-xor (bit-shift-left l 1)
           (bit-shift-right l 63)))

(defmethod to-zigzag
  java.lang.Integer [i]
  (bit-xor (bit-shift-left i 1)
           (bit-shift-right i 31)))

(def sig-bits
  "Contains a map with key int values 0-7 with value the bit pattern for so many significant bits. Usefull to get the value out of the least significant bits for x nr of bits"
  (atom 
   (->> (range 8)
        (map #(list % (dec (math/expt 2 %))))
        flatten
        (map unchecked-byte)
        (apply hash-map))))

(defmulti varint-encode
  "Encodes an integral value into varint form"
  type)

(defn varint-encode-bits
  "var-int encodes the least significant bits bits of l"
  [l bits]
  (letfn [(make-last [x sigs]
            (-> (get @sig-bits sigs)
                (bit-and x)
                (bit-or 128)
                unchecked-byte))]
  (loop [bits (- bits 7)
         nr (-> (bit-shift-right l 7)
                (bit-and (-> (math/expt 2 (- bits 8)) ;; clear out top-most 7 bits to prevent filling with 1
                             dec)))
         out []
         last (make-last l 7)]
    (if (or (= 0 nr)
            (<= bits 0))
      (conj out
            (bit-and last 127))
      (recur (- bits 7)
             (bit-shift-right nr 7)
             (conj out last)
             (make-last nr (min 7 bits)))))))

        
(defmethod varint-encode
  java.lang.Long [l]
  (varint-encode-bits l 64))

(defmethod varint-encode
  java.lang.Integer [i]
  (varint-encode-bits i 32))

;; (defmulti encode-value
;;   "Takes a language value and encodes it in protobuf-serialized format"
;;   )

;; (defmethod encode-value
;;   :default [_]
;;   (println "default"))


;; (defmethod encode-value
;;   [java.lang.Long :int64] [x]
  
;;   (println (str x)))

(defn make-encoding-value
  "Produces the byte that goes ahead of a value in a protobuf stream. Contains information about the field number and the field value type"
  [nr tag]
   (let [tagnr ({:varint 0
                :64-bit 1
                :length-delimited 2
                :start-group 3
                :end-group 4
                :32-bit 5}
               tag)]
    (-> (+ (bit-shift-left nr 3)
           tagnr)
        varint-encode)))
        




