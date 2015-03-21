(ns rksm.cloxp-repl.live-eval-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.live-eval :refer :all]))

(deftest eval-code-and-gather-results
  
  (testing "eval statements"
   (let [code "(def x 23)\n\n(+ x 2)\n3"
         {:keys [defs results]} (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "",
              :source "(def x 23)\n" :value "x => 23", :def? true}
             {:pos {:line 3, :column 1}, :out "", :value "25", :source "(+ x 2)\n", :def? false}
             {:pos {:line 4, :column 1}, :out "", :value "3", :source "3\n", :def? false}]
            results))
     (is (= ['x] (keys defs)))))

  (testing "with errors"
    (let [code "(/ 1 0)"
         {:keys [defs results]} (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "",
              :source "(/ 1 0)\n", :def? false,
              :value "#<ArithmeticException java.lang.ArithmeticException: Divide by zero>"}]
            results))))
  
  (testing "stdout"
    (let [code "(pr 123)"
         {:keys [defs results]} (live-eval-code code :ns 'user)]
     (is (= [{:pos {:line 1, :column 1}, :out "123",
              :source "(pr 123)\n", :def? false, :value "nil"}]
            results)))))

(deftest generates-def-changes
  (let [tests [{:code "(def x 23)\n\n(+ x 2)\n3"
                :expected {:added #{'x}, :removed #{}, :changed #{}}}
               {:code "(def x 23)\n\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{}, :changed #{}}}
               {:code "(def x 24)\n\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{}, :changed #{'x}}}
               {:code "\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{'x}, :changed #{}}}]]
    (loop [def-state {} tests tests]
      (when-not (empty? tests)
        (let [[{:keys [code expected]} & rest] tests
              {:keys [changes results] :as new-state}
              (live-eval-code-with-changes code :ns 'user :env def-state)]
          (is (= expected changes) code)
          (recur new-state rest))))))

(deftest disappearing-defs-are-removed
  (let [tests [{:code "(def x 23)"
                :expected {:added #{'x}, :removed #{}, :changed #{}}}
               {:code "3"
                :expected {:added #{}, :removed #{'x}, :changed #{}}}]]
    (loop [def-state {} tests tests]
      (when-not (empty? tests)
        (let [[{:keys [code expected]} & rest] tests
              {:keys [changes results] :as new-state}
              (live-eval-code-with-changes code :ns 'user :env def-state)]
          (is (= expected changes) code)
          (recur new-state rest))))
    (is (nil? (-> 'user ns-interns (get 'x))))))

(deftest keeping-changes-test
  (let [tests [{:code "(def x 23)\n\n(+ x 2)\n3"
                :expected {:added #{'x}, :removed #{}, :changed #{}}}
               {:code "(def x 23)\n\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{}, :changed #{}}}
               {:code "(def x 24)\n\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{}, :changed #{'x}}}
               {:code "\n(+ x 2)\n3"
                :expected {:added #{}, :removed #{'x}, :changed #{}}}]]
    (doseq [t tests]
      (let [{:keys [code expected]} t
            {:keys [changes results]} (live-eval-code-keeping-env
                                       code
                                       :ns 'user
                                       :id 'keeping-changes-test
                                       :reset-timeout 100)]
        (is (= expected changes))
        (Thread/sleep 50)))
    (is (not (nil? (get @envs 'keeping-changes-test))))
    (Thread/sleep 110)
    (is (nil? (get @envs 'keeping-changes-test)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests *ns*)
 (live-eval-code "(/ 1 0)" :ns 'user)
 )