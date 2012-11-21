(ns parsering.core
  (:use [clojure.pprint]
        [clojure.tools.cli :only [cli]])
  (:require [parsering.lexer :as lex])
  (:gen-class))


(defn show-help
  []
  (println "specify the name of the app you want to run."))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (condp = (first args)
    "help" (show-help)
    "lex" (lex/main (rest args))
    (println "unknown appname")))
