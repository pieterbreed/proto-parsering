(ns proto.decoder
  (:use [proto.common])
  (:require [proto.types :as ptypes]))

;; utilities

(defn from-zigzag
  "Reverse zigzag, takes an integral encoded in zigzag form and produces a value encoded in normal 2s complement form"
  [l]
  (bit-xor (bit-shift-right l 1)
           (* -1 (bit-and l 1))))

;; ----------------------------------------
;; about decoding things encoded as varints

(defmulti varint-decode
  "Decodes a varint value based on :enc-type"
  :enc-type)

(defmethod varint-decode :int32 [x] (unchecked-int (:value x)))
(defmethod varint-decode :int64 [x] (:value x))
(defmethod varint-decode :uint32 [x] (unchecked-int (:value x)))
(defmethod varint-decode :uint64 [x] (:value x))
(defmethod varint-decode :sint32 [x] (unchecked-int (from-zigzag (:value x))))
(defmethod varint-decode :sint64 [x] (from-zigzag (:value x)))
(defmethod varint-decode :bool [x] (if (= (:value x) 0)
                                     false true))

;; ----------------------------------------------------
;; about decoding things encoded as fixed 64-bit values

(defmulti sixty-four-bit-decode
  "decodes a 64-bit value based on :enc-type"
  :enc-type)

(defmethod sixty-four-bit-decode :fixed64 [x] (:value x))
(defmethod sixty-four-bit-decode :sfixed64 [x] (from-zigzag (:value x)))
(defmethod sixty-four-bit-decode :double [x] (java.lang.Double/longBitsToDouble (:value x)))

;; ---------------------------------------------------
;; about decoding things encoded as fixed 32-bit values

(defmulti thirty-two-bit-decode
  "decodes a 32 bit value based on :enc-type"
  :enc-type)

(defmethod thirty-two-bit-decode :fixed32 [x] (-> x :value unchecked-int))
(defmethod thirty-two-bit-decode :sfixed32 [x] (-> x :value from-zigzag unchecked-int))
(defmethod thirty-two-bit-decode :float [x]
  (-> x :value unchecked-int java.lang.Float/intBitsToFloat))

;; -----------------------------------------------------------------------
;; about decoding things which are length-delimited (byte arrays basically)

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

;; -----------------------------------------
;; pulling things together, decoding a value

(defmulti value-decode
  "Decodes a value based on the type"
  :type)

(defmethod value-decode :varint [x] (varint-decode x))
(defmethod value-decode :64-bit [i] (sixty-four-bit-decode i))
(defmethod value-decode :32-bit [x] (thirty-two-bit-decode x))
(defmethod value-decode :length-delimited [x] (length-delimited-decode x))

;; --------------------------------------------------------------------------
;; unzipping functions (a byte sequence is being unwrapped as we go along it)
;; each function returns a vector with [bs result] updated

(defn unzip-varint
  "unzips a varint from the stream, returns the rest of the bytes and the single value"
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

(defn unzip-length-delimited
  "unzips a length-delimited amount of bytes from the stream"
  [bs]
  (let [[bs length] (unzip-varint bs)]
    [(drop length bs)
     (take length bs)]))

(defn unzip-fixed
  "unzips a specified nr of bytes from the stream"
  [bs i]
  [(drop i bs)
   (-> (take i bs)
       (reduce #(+ (bit-shift-left %1 8) ;; comes in little-endian byte order
                   %2)))])
  
(defn unzip-sixty-four-bit
  "unzips a 64-bit value from the stream"
  [bs]
  (unzip-fixed bs 8))

(defn unzip-thirty-two-bit
  "unzips a 32-bit int value from the stream"
  [bs]
  (unzip-fixed bs 4))

(defn unzip-tag
  "unzips a tag value from the stream stream of bytes to determine the next fields tag nr and field type. Returns the stream of bytes (with the used values removed) and the wire type and tag nr"
  [bs]
  (let [[bseq tag] (unzip-varint bs)
        type (bit-and 7 tag)
        tag-nr (bit-shift-right tag 3)]
    [bseq {:type :encoding-marker
           :wire-type ({0 :varint
                        1 :64-bit
                        2 :length-delimited
                        3 :start-group
                        4 :end-group
                        5 :32-bit} type)
           :tag-nr tag-nr}]))

(defn decode-from-stream
  "Decodes one value from a stream of bytes and conj it onto out"
  [bs out]
  (let [[bs tag] (unzip-tag bs)
        wire-type (:wire-type tag)
        [bs val] (condp = wire-type
                   :varint (unzip-varint bs)
                   :length-delimited (unzip-length-delimited bs)
                   :64-bit (unzip-sixty-four-bit bs)
                   :32-bit (unzip-thirty-two-bit bs)
                   )] ;; implied IllegalArgumentException when an unknownn wire-type is encountered (eg start-group)
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


