(ns parsering.common
  (:require [clojure.java.io :as io])
  (:use     [clojure.pprint :only [pprint]]))

(defn copy-meta
  "if x has meta data with value parsering-data it will copy and merge it into the meta-data of y"
  [x y]
  (let [parsering-x (get (meta x) :parsering-data {})
        meta-y (meta y)
        result (merge meta-y {:parsering-data parsering-x})]
    (with-meta y result)))
       
(defn create-file-loader
  "Creates a fn that closes over the working path and returns a function that can resolve filenames into streams of characters"
  [path]
  #(slurp (io/file path %)))

(defn directory?
  "Checks whether the string specified as a valid directory"
  [path]
  (-> (java.io.File. path)
      .isDirectory))

(defn parse-directory
  "Checks whether the passed str is a valid directory and returns it if it is, otherwise nil"
  [path]
  (let [d (java.io.File. path)]
    (if (.isDirectory d)
      d
      nil)))

(defn debug-print
  [x]
  (pprint x)
  x)

  