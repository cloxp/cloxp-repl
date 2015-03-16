(ns rksm.cloxp-repl-cljx-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.cljx :refer [enable-cljx-load-support! disable-cljx-load-support!]]
            ; [rksm.cloxp-source-reader.core :as src-rdr]
            ; [rksm.cloxp-repl :refer :all]
            ))

(deftest cljx-can-be-required
  (enable-cljx-load-support!)
  (require 'rksm.resources.dummy-1 :reload)
  (is (= ['x-to-string] (keys (ns-interns 'rksm.resources.test.dummy-1))))
  (remove-ns 'rksm.resources.dummy-1)
  (disable-cljx-load-support!)
  (is (thrown-with-msg? java.io.FileNotFoundException #"Could not locate"
                        (require 'rksm.resources.dummy-1 :reload)))
  )

(comment
 (keep (comp (partial re-find #".*repl.*") str) (rksm.system-files/classpath))
 (run-tests *ns*)
  )
