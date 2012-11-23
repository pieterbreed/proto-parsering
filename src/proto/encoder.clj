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
  [enc-type-name & rest]
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

(defmulti value-decode
  "Decodes a value based on the type"
  :type)

(defmulti varint-decode
  "Decodes a varint value based on :enc-type"
  :enc-type)

(defmethod varint-decode :int32 [x] (unchecked-int (:value x)))
(defmethod varint-decode :int64 [x] (:value x))
(defmethod varint-decode :uint32 [x] (unchecked-int (:value x)))
(defmethod varint-decode :uint64 [x] (:value x))
(defmethod varint-decode :sint32 [x] (unchecked-int (from-zigzag (:value x))))
(defmethod varint-decode :sint64 [x] (from-zigzag (:value x)))
(defmethod varint-decode :bool [x] (condp = (:value x)
                                     0 false
                                     true))

(defmethod value-decode
  :varint [x] (varint-decode x))


(defmulti sixty-four-bit-decode
  "decodes a 64-bit value based on :enc-type"
  :enc-type)

(defmethod sixty-four-bit-decode :fixed64 [x] (:value x))
(defmethod sixty-four-bit-decode :sfixed64 [x] (from-zigzag (:value x)))
(defmethod sixty-four-bit-decode :fixed64 [x] (java.lang.Double/longBitsToDouble (:value x)))

(defmethod value-decode
  :64-bit [i] (sixty-four-bit-decode i))

(defmulti length-delimited-decode
  "decodes a length-delimited value based on enc-type"
  :enc-type)

(defmethod length-delimited-decode
  :string [x]
  (let [byts (->> x
                  :value
                  (map unchecked-byte)
                  byte-array)]
    (new java.lang.String byts "UTF8")))

(defmethod length-delimited-decode
  :bytes [xs]
  (->> xs
       :value
       (map unchecked-byte)
       byte-array))

(defmulti thirty-two-bit-decode
  "decodes a 32 bit value based on :enc-type"
  :enc-type)

(defmethod thirty-two-bit-decode :fixed32 [x] (-> x :value unchecked-int))
(defmethod thirty-two-bit-decode :sfixed32 [x] (-> x :value from-zigzag unchecked-int))
(defmethod thirty-two-bit-decode :float [x]
  (-> x :value unchecked-int java.lang.Float/intBitsToFloat))

(defmethod value-decode
  :32-bit [x]
  (thirty-two-bit-decode x))

(defmethod value-decode
  :length-delimited [x] (length-delimited-decode x))

(defmulti varint-encode
  "Encodes an integral value into varint form"
  type)

(def sig-bits
  "Contains a map with key int values 0-7 with value the bit pattern for so many significant bits. Usefull to get the value out of the least significant bits for x nr of bits"
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
          (vector (rest bseq) (reduce reducefn byt parts))
          (recur (rest bseq)
                 (conj parts byt)))))))
        
(defmethod varint-encode
  java.lang.Long [l]
  (varint-encode-bits l 64))

(defmethod varint-encode
  java.lang.Integer [i]
  (varint-encode-bits i 32))

(defn length-delimited-decode-bits
  "decodes a length-delimited value into a sequence of bytes"
  [bs]
  (let [[bs length] (varint-decode-bits bs)]
    [(drop length bs)
     (take length bs)]))

(defn fixed-bits-decode-bits
  "Decodes a fixed number of bytes from the stream and returns a long with the value. Little-endian byte order is assumed in the stream"
  [bs i]
  [(drop i bs)
   (-> (take i bs)
       (reduce #(+ (bit-shift-left %1 8) ;; comes in little-endian byte order
                   %2)))])
  

(defn sixty-four-bit-decode-bits
  "Decodes a 64-bit value from the stream"
  [bs]
  (fixed-bits-decode-bits bs 8))

(defn thirty-two-bit-decode-bits
  "Decodes a 32-bit int value from the stream"
  [bs]
  (fixed-bits-decode-bits bs 4))

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
        

(defn decode-encoding-value
  "Decodes the stream of bytes to determine the next fields tag nr and field type. Returns the stream of bytes (with the used values removed) and the wire type and tag nr"
  [bs]
  (let [[bseq tag] (varint-decode-bits bs)]
    [bseq {:type :encoding-marker
           :wire-type ({0 :varint
                        1 :64-bit
                        2 :length-delimited
                        3 :start-group
                        4 :end-group
                        5 :32-bit} (bit-and 7 tag))
           :tag-nr (bit-shift-right tag 3)}]))

(defn decode-from-stream
  "Decodes one value from a stream of bytes and conj it onto out"
  [bs out]
  (let [[bs tag] (decode-encoding-value bs)
        wire-type (:wire-type tag)
        [bs val] (condp = wire-type
                   :varint (varint-decode-bits bs)
                   :length-delimited (length-delimited-decode-bits bs)
                   :64-bit (sixty-four-bit-decode-bits bs)
                   :32-bit (thirty-two-bit-decode-bits bs)
                   )]
    (vector bs (conj out
                     {:type wire-type
                      :value val
                      :tag-nr (:tag-nr tag)}))))


(defn decode-protobuf-stream
  "Decodes a stream of integers assumed to be in protobuf-encoded format until the string is consumed"
  [bs]
  (loop [byts bs
         out []]
    (if (nil? (first byts))
      out
      (let [[rem step-result] (decode-from-stream byts out)]
        (recur rem step-result)))))


;; (def data [0x12 0x07 0x74 0x65 0x73 0x74 0x69 0x6e 0x67])  
  
    
;; (def data [10, 24, 99, 111, 109, 46, 97, 108, 108, 97, 110, 103, 114, 97, 121, 46, 109, 111, 100, 101, 108, 46, 84, 101, 115, 116, 32, 1, 40, 0])