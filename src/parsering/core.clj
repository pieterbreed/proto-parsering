(ns parsering.core
  (:use [clojure.pprint]
        [clojure.tools.cli :only [cli]]
        [clojure.core])
  (:require [parsering.lexer :as lex])
  (:gen-class))


(defn show-help
  []
  (println "specify the name of the app you want to run."))

(defn fs-file [name]
  {:where :file-system
   :file-name name})

(defmulti resolve-file
  "Resolves a filename string into a string of its contents"
  :where)

(defmethod resolve-file
  :file-system [name]
  (slurp (:file-name name)))

(defn create-fs-resolver
  "Creates a file system file resolver based on a folder"
  [folder-location]
  (let [folderFile (new java.io.File folder-location)]
    #(-> (fs-file (-> (new java.io.File folderFile %)
                      .getAbsolutePath))
         resolve-file)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (condp = (first args)
    "help" (show-help)
    "lex" (lex/main (rest args))
    (println "unknown appname")))
