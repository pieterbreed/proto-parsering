(ns proto.decoder2
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser varint-item [] (token #(= (bit-and 128 %) 128)))
(defparser varint-term [] (token #(= (bit-and 128 %) 0)))

(defparser varint []
  (letfn [(reducefn [result byt]
            (+ (bit-and byt 127)
               (bit-shift-left result 7)))]
    (let->> [non-terms (many (varint-item))
             term (varint-term)]
            (always (reduce reducefn term non-terms)))))

(defparser value []
  (token number?))

(defparser length-delimited []
  (let->> [length (varint)
           bs (times length (value))]
          (always bs)))

(defparser fixed-integral [nr]
  (let->> [bs (times nr (value))]
          (always (reduce #(+ (bit-shift-left %1 8) %2) (reverse bs)))))

(defparser fixed-64 []
  (fixed-integral 8))

(defparser fixed-32 []
  (fixed-integral 4))

(defparser tag []
  (let->> [tag (varint)]
          (let [type (bit-and 7 tag)
                tag-nr (bit-shift-right tag 3)]
            (always {:wire-type ({0 :varint
                                  1 :64-bit
                                  2 :length-delimited
                                  3 :start-group
                                  4 :end-group
                                  5 :32-bit} type)
                     :tag-nr tag-nr}))))

(defparser item
  []
  (let->> [t (tag)
           val (condp (:wire-type t) =
                 :varint (varint)
                 :64-bit (fixed-64)
                 :length-delimited (length-delimited)
                 :32-bit (fixed-32))]
          (always (assoc t :value val))))

(defparser proto-stream
  []
  (let->> [body (many1 (item))
           _ (eof)]
  (always body)))