(ns proto.encoder
  (:require [clojure.math.numeric-tower :as math]))

(defn show-byte-bits
  "Computes the bit pattern of a single byte"
  [byt]
  (loop [shifts-left 8
         result '()
         res byt]
    (let [b (bit-and res 1)]
      (if (= 0 shifts-left)
        result
        (recur (dec shifts-left)
               (conj result b)
               (bit-shift-right res 1))))))

(defonce bit-pattern-lookup
  (atom (loop [bits {}
               bytes {}
               i 0]
          (if (= i 256)
            {:from-bytes bits
             :from-bits bytes}
            (let [bit-pattern (show-byte-bits i)]
              (recur (assoc bits
                       i bit-pattern)
                     (assoc bytes
                       bit-pattern i)
                     (inc i)))))))

(def byte-to-bits
  "([b])
   Returns a possible byte value (0 <= b <= 255) as a list of bits"
  (memoize (fn [b]
             {:pre [(and (<= b 255)
                         (<= 0 b))]}
             (-> @bit-pattern-lookup
                 :from-bytes
                 (get b)))))

(def bits-to-byte
  "([bits])
   Interprets a list with 8 items, each either 0 or 1, as a bit pattern, big-endian bit order as a byte value and returns it"
  (memoize (fn [bits]
             {:pre [(and (= 8 (count bits))
                         (every? #(or (= 1 %)
                                      (= 0 %)) bits))]}
             [bits]
             (-> @bit-pattern-lookup
                 :from-bits
                 (get bits)))))

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

(defn from-zigzag
  "Reverse zigzag, takes an integral encoded in zigzag form and produces a value encoded in normal 2s complement form"
  [l]
  (bit-xor (bit-shift-right l 1)
           (* -1 (bit-and l 1))))

(defn map-bytes
  "Performs map operation against the n least significant bytes in the integral value x"
  [f x n]
  (loop [left n
         i x
         out '()]
    (if (= 0 left)
      out
      (recur (dec left)
             (bit-shift-right i 8)
             (conj out (f (bit-and i 255)))))))

(defn show-n-bytes-as-bits
  "Interprets value x as an integral value, and takes the least significant n bytes of it and displays them"
  [x n]
  (map-bytes byte-to-bits x n))

(defmacro create-enc-type
  [enc-type-name]
  `(defn ~enc-type-name [x#] {:value x# :enc-type (keyword ~(str enc-type-name))}))

(create-enc-type double)
(create-enc-type float)
(create-enc-type int32)
(create-enc-type int64)
(create-enc-type uint32)
(create-enc-type uint64)
(create-enc-type sint32)
(create-enc-type sint64)
(create-enc-type fixed32)
(create-enc-type fixed64)
(create-enc-type sfixed32)
(create-enc-type sfixed64)
(create-enc-type bool)
(create-enc-type string)
(create-enc-type bytes)

(defn discriminate
  "Returns a map with properties of x"
  [value-and-enc-type]
    {:type (type (:value value-and-enc-type))
     :enc-type (:enc-type value-and-enc-type)})

(defmulti show-as-bits
  "Show a value as a sequence of bits"
  discriminate)

(defmethod show-as-bits
  {:type java.lang.Long, :enc-type :int64}
  [l]
  (show-n-bytes-as-bits (:value l) 8))

(defmethod show-as-bits
  {:type java.lang.Integer
   :enc-type :int32}
  [i]
  (show-n-bytes-as-bits (:value i) 4))

(defmethod show-as-bits
  {:type java.lang.Long
   :enc-type :int32}
  [l]
  (show-as-bits (int32 (.intValue (:value l)))))

(defmethod show-as-bits
  {:type java.lang.Double
   :enc-type :double}
  [d]
  (show-n-bytes-as-bits (:value (java.lang.Double/doubleToLongBits d) 8)))

(defmethod show-as-bits
  {:type java.lang.Float
   :enc-type :float}
  [d]
  (show-n-bytes-as-bits (java.lang.Float/floatToIntBits (:value d)) 4))

(defmethod show-as-bits
  {:type java.lang.Double
   :enc-type :float}
  [d]
  (show-as-bits (float (.floatValue (:value d)))))

(defmulti varint-encode
  "Encodes an integral value into varint form"
  type)

(def sig-bits
  (atom 
   (->> (range 8)
        (map #(list % (dec (math/expt 2 %))))
        flatten
        (map unchecked-byte)
        (apply hash-map))))



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

(defn varint-decode-bits
  "var-int decodes the provided seq of bytes into a long value"
  [bs]
  (letfn [(reducefn [result byt]
            (+ (bit-and byt 127)
               (bit-shift-left result 7)))]
    (loop [bseq bs
           parts '()]
      (let [byt (first bseq)]
        (if (-> (bit-and 128 byt) (= 0))
          (reduce reducefn byt parts)
          (recur (rest bseq)
                 (conj parts byt)))))))
        

(defmethod varint-encode
  java.lang.Long [l]
  (varint-encode-bits l 64))

(defmethod varint-encode
  java.lang.Integer [i]
  (varint-encode-bits i 32))

(defmulti encode-value
  "Takes a language value and encodes it in protobuf-serialized format"
  )

(defmethod encode-value
  :default [_]
  (println "default"))


(defmethod encode-value
  [java.lang.Long :int64] [x]
  
  (println (str x)))
     