(defproject logic "0.1.0-SNAPSHOT"
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.7.0-alpha6"]
                 [org.clojure/core.logic "0.8.10"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.10"]]}})
