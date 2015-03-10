(ns rksm.cloxp-repl.live-eval-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.live-eval :refer :all]))

(deftest eval-code-and-gather-results
  
  (testing "eval statements"
   (let [code "(def x 23)\n\n(+ x 2)\n3"
         eval-result (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "",
              :value "x => 23"}
             {:pos {:line 3, :column 1}, :out "", :value "25"}
             {:pos {:line 4, :column 1}, :out "", :value "3"}]
            eval-result))))

  (testing "with errors"
    (let [code "(/ 1 0)"
         eval-result (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "",
              :value "#<ArithmeticException java.lang.ArithmeticException: Divide by zero>"}]
            eval-result))))
  
  (testing "stdout"
    (let [code "(pr 123)"
         eval-result (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "123",
              :value "nil"}]
            eval-result))))
  )

(comment
 (run-tests *ns*)
 (live-eval-code "(/ 1 0)" :ns 'user)
 )