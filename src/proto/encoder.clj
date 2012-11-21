(ns proto.encoder)

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

(defn show-n-bytes-as-bits
  "Interprets value x as an integral value, and takes the least significant n bytes of it and displays them"
  [x n]
  (loop [left n
         i x
         out '()]
    (if (= 0 left)
      out
      (recur (dec left)
             (bit-shift-right i 8)
             (conj out (byte-to-bits (bit-and i 255)))))))

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

(defmulti encode-value
  "Takes a language value and encodes it in protobuf-serialized format"
  )


  

(defmethod encode-value
  :default [_]
  (println "default"))


(defmethod encode-value
  [java.lang.Long :int64] [x]
  
  (println (str x)))
     