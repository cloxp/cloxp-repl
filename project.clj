(defproject org.rksm/cloxp-repl "0.1.7"
  :description "Source reading, parsing, and querying for cloxp."
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.nrepl "0.2.10"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [clj-stacktrace/clj-stacktrace "0.2.8"]
                 [org.rksm/cloxp-source-reader "0.1.8"]]
  :source-paths ["src/main"]
  :test-paths ["src/test"]
  :scm {:url "git@github.com:cloxp/cloxp-repl.git"}
  :pom-addition [:developers [:developer
                              [:name "Robert Krahn"]
                              [:url "http://robert.kra.hn"]
                              [:email "robert.krahn@gmail.com"]
                              [:timezone "-9"]]])
