(ns rksm.cloxp-repl.live-eval-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.live-eval :refer :all]))

(defonce this-file (-> *ns* 
                     ns-name munge
                     (clojure.string/replace #"\." "/")
                     (str ".clj")
                     clojure.java.io/resource .getFile))

(defn fixture [test]
  (test)
  (ns-unmap 'user 'x)
  (ns-unmap 'user 'y))

(use-fixtures :each fixture)

(deftest eval-code-and-gather-results
  
  (testing "eval statements"
   (let [code "(def x 23)\n\n(+ x 2)\n3"
         results (map (juxt :printed :out :error)
                      (live-eval-code code :ns 'user))]
     (is (= [["x => 23" "" nil] ["25" "" nil] ["3" "" nil]] results))))

  (testing "with errors"
    (let [code "(/ 1 0)"
          results (map :printed (live-eval-code code :ns 'user))]
     (is (= ["java.lang.ArithmeticException: Divide by zero"]
            results))))
  
  (testing "stdout"
    (let [results (map (juxt :printed :out) (live-eval-code "(pr 123)" :ns 'user))]
      (is (= [["nil" "123"]] results)))))

(deftest keeping-changes-test
  (binding [*file* this-file]
    (let [tests [{:code "(def x 23)\n\n(+ x 2)\n3"
                  :expected ["x => 23" "25" "3"]}
                 {:code "(def x 23)\n\n(+ x 2)\n3"
                  :expected ["x => 23" "25" "3"]}
                 {:code "(def x 24)\n\n(+ x 2)\n3"
                  :expected ["x => 24" "26" "3"]}
                 {:code "\n(+ x 2)\n3"
                  :expected [(str "java.lang.RuntimeException: Unable to resolve symbol: x in this context, compiling:(" this-file ":2:1)") "3"]}]]
      (doseq [t tests]
        (let [{:keys [code expected]} t
              results (map :printed (live-eval-code-keeping-env
                                     code
                                     :ns 'user
                                     :id 'keeping-changes-test
                                     :reset-timeout 100))]
          (is (= expected results))
          (Thread/sleep 50)))
      (is (not (nil? (get @envs 'keeping-changes-test))))
      (Thread/sleep 110)
      (is (nil? (get @envs 'keeping-changes-test))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (let [w (java.io.StringWriter.)]
   (binding [*test-out* w]
    (run-tests *ns*)
     (print (str w))))

 (live-eval-code "(/ 1 0)" :ns 'user)
 )