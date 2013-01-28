(ns debug.debug)

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
  "performs (format) on str and args and writes the result to *out* if *debug* is not false"
  (if *debug*
    (let [res (apply format fmt args)]
      (println res)
      res)
    nil))

(defmacro debug
  [fmt & args]
  (if *make-debug-code*
    `(apply debug-fn ~fmt [~@args])))
