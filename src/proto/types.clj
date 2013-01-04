(ns proto.types
  (:refer-clojure :exclude [double float bytes])
  (:use [proto.common]))

(defmacro create-proto-type
  [enc-type-name & rest]
  `(defn ~enc-type-name [x#] {:value x# :enc-type (keyword ~(str enc-type-name))}))

(create-proto-type double)
(create-proto-type float)
(create-proto-type int32)
(create-proto-type int64)
(create-proto-type uint32)
(create-proto-type uint64)
(create-proto-type sint32)
(create-proto-type sint64)
(create-proto-type fixed32)
(create-proto-type fixed64)
(create-proto-type sfixed32)
(create-proto-type sfixed64)
(create-proto-type bool)
(create-proto-type string)
(create-proto-type bytes)

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
  (n-bytes-as-bits (:value l) 8))

(defmethod show-as-bits
  {:type java.lang.Integer
   :enc-type :int32}
  [i]
  (n-bytes-as-bits (:value i) 4))

(defmethod show-as-bits
  {:type java.lang.Long
   :enc-type :int32}
  [l]
  (show-as-bits (int32 (.intValue (:value l)))))

(defmethod show-as-bits
  {:type java.lang.Double
   :enc-type :double}
  [d]
  (n-bytes-as-bits (:value (java.lang.Double/doubleToLongBits d) 8)))

(defmethod show-as-bits
  {:type java.lang.Float
   :enc-type :float}
  [d]
  (n-bytes-as-bits (java.lang.Float/floatToIntBits (:value d)) 4))

(defmethod show-as-bits
  {:type java.lang.Double
   :enc-type :float}
  [d]
  (show-as-bits (float (.floatValue (:value d)))))
