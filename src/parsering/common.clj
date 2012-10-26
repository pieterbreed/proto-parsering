(ns parsering.common)

(defn copy-meta
  "if x has meta data with value parsering-data it will copy and merge it into the meta-data of y"
  [x y]
  (let [parsering-x (get (meta x) :parsering-data {})
        meta-y (meta y)
        result (merge meta-y {:parsering-data parsering-x})]
    (with-meta y result)))
       
      
  