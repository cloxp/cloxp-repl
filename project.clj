(defproject org.rksm/cloxp-repl "0.1.1-SNAPSHOT"
  :description "Source reading, parsing, and querying for cloxp."
  :license "MIT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.nrepl "0.2.7"]
                 [org.rksm/cloxp-source-reader "0.1.0-SNAPSHOT"]]
  :source-paths ["src/main"]
  :test-paths ["src/test"]
  :scm {:url "git@github.com:cloxp/cloxp-repl.git"}
  :pom-addition [:developers [:developer
                              [:name "Robert Krahn"]
                              [:url "http://robert.kra.hn"]
                              [:email "robert.krahn@gmail.com"]
                              [:timezone "-9"]]])
