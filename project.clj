(defproject parsering "0.1.0-SNAPSHOT"
  :description "messing around with the parsatron to parse protobuf definition files"
;  :url "http://example."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [the/parsatron "0.0.3"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/math.numeric-tower "0.0.1"]]
  :main parsering.core)
