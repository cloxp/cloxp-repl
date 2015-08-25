(ns rksm.cloxp-repl.multi-writer-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.MultiWriter :as writing])
  (:import (rksm.cloxp-repl MultiWriter)
           (java.io StringWriter)))

(deftest write-to-different-writers
  
  (let [writer-1 (StringWriter.)
        writer-2 (StringWriter.)
        writer-3 (StringWriter.)
        writer (MultiWriter. writer-1)]
    
    (binding [*out* writer]
      
      (testing "base writer"
        (println "test-1")
        (is (= "test-1\n" (str writer-1))))
      
      (testing "add a writer"
        (.pushWriter writer writer-2)
        (println "test-2")
        (is (= "test-1\n" (str writer-1)))
        (is (= "test-2\n" (str writer-2))))
      
      (testing "remove writer"
        (is (= writer-2 (.popWriter writer)))
        (println "test-3")
        (is (= "test-1\ntest-3\n" (str writer-1)))
        (is (= "test-2\n" (str writer-2)))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (let [w (java.io.StringWriter.)]
   (binding [*test-out* w]
    (run-tests *ns*)
     (print (str w))))
 )
