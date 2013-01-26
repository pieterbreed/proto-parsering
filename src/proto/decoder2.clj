(ns proto.decoder2
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(def ^:dynamic *debug* false)

(def *make-debug-code*
  "influences the debug macro. If this value is true the code that generates the calls to debug is generated, otherwise it is left out of the syntax tree altogether, as is appropriate for run-time"
  true)
;; this must go to false when code is checked in
;; otherwise extra code is being generated that is
;; only usefull for debugging

(defmacro with-debugging
  "Turns debugging output on to *out* while parsing byte streams only for the context of forms"
  [& forms]
  `(binding [*debug* true]
     ~@forms))

(defn debug-fn
  [fmt & args]
  "performs (format) on str and args and writes the result to *out* of *debug* is not false"
  (if *debug*
    (let [res (apply format fmt args)]
      (println res)
      res)
    nil))

(defmacro debug
  [fmt & args]
  (if *make-debug-code*
    `(apply debug-fn ~fmt ~@args)))

(defparser varint-item [] (token #(let [is-item (= (bit-and 128 %) 128)]
                                    (debug "varint-item test = %b")
                                    is-item)))
(defparser varint-term [] (token #(let [is-term (= (bit-and 128 %) 0)]
                                    (debug "varint-term test = %b")
                                    is-term)))

(defparser varint []
  (letfn [(reducefn [result byt]
            (+ (bit-and byt 127)
               (bit-shift-left result 7)))]
    (let->> [non-terms (many (varint-item))
             term (varint-term)]
            (debug "all the non-term varints are: %s" (reduce #(str %1 " " %2) non-terms))
            (debug "the terminator for the varint is: %s" (str term))
            (let [result (reduce reducefn term non-terms)]
              (debug "varint value : %s" result)
              (always result)))))

(defparser value []
  (token number?))

(defparser length-delimited []
  (let->> [length (varint)
           bs (times length (value))]
          (debug "varint bytes read (total of %d) = %s"
                 length
                 (reduce #(str %1 " " %2)))
          (always bs)))

(defparser fixed-integral [nr]
  (let->> [bs (times nr (value))]
          (let [result (reduce #(+ (bit-shift-left %1 8) %2) (reverse bs))]
            (debug "read %d items for a fixed integral of value %d"
                   nr
                   result)
            (always result))))

(defparser fixed-64 []
  (fixed-integral 8))

(defparser fixed-32 []
  (fixed-integral 4))

(defparser tag []
  (let->> [tag (varint)]
          (debug "tag value is '%d'" tag)
          (let [type (bit-and 7 tag)
                _ (debug "type of the tag is '%d'" type)
                tag-nr (bit-shift-right tag 3)
                _ (debug "tag number is '%d'" tag-nr)
                result {:wire-type ({0 :varint
                                  1 :64-bit
                                  2 :length-delimited
                                  3 :start-group
                                  4 :end-group
                                  5 :32-bit} type)
                        :tag-nr tag-nr}]
            (debug "full result from parsing the tag: %s" (str result))
            (always result))))

(defparser item
  []
  (let->> [t (tag)
           val (condp (:wire-type t) =
                 :varint (varint)
                 :64-bit (fixed-64)
                 :length-delimited (length-delimited)
                 :32-bit (fixed-32))]
          (debug "parsing item: tag value = %s\nvalue = %s"
                 (str t)
                 (str val))
          (always (assoc t :value val))))

(defparser proto-stream
  []
  (let->> [body (many1 (item))
           _ (eof)]
  (always body)))