(defproject org.rksm/cloxp-repl "0.1.4"
  :description "Source reading, parsing, and querying for cloxp."
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.nrepl "0.2.7"]
                 [org.clojure/tools.namespace "0.2.8"]
                 [clj-stacktrace/clj-stacktrace "0.2.8"]
                 [com.keminglabs/cljx "0.6.0"]
                 [org.rksm/cloxp-source-reader "0.1.4"]]
  :source-paths ["src/main"]
  :test-paths ["src/test"]
  :scm {:url "git@github.com:cloxp/cloxp-repl.git"}
  :pom-addition [:developers [:developer
                              [:name "Robert Krahn"]
                              [:url "http://robert.kra.hn"]
                              [:email "robert.krahn@gmail.com"]
                              [:timezone "-9"]]])
