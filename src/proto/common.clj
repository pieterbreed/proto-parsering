(ns proto.common)

(defn byte-to-bits-seq
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
            (let [bit-pattern (byte-to-bits-seq i)]
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

(defn n-bytes-as-bits
  "Interprets value x as an integral value, and takes the least significant n bytes of it and displays them"
  [x n]
  (map-bytes byte-to-bits x n))

