(ns rksm.cloxp-repl-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl :refer :all]))

(defn fixture [test]
  (test)
  (ns-unmap *ns* 'x)
  (ns-unmap *ns* 'y))

(use-fixtures :each fixture)


(deftest reading
  
  (testing "simple read"
    (is (= [{:form '(ns rksm.cloxp-repl-test.dummy-3),
             :source "(ns rksm.cloxp-repl-test.dummy-3)",
             :line 1,
             :column 1}
            {:form '(def x 23), :source "(def x 23)", :line 2, :column 3}]
           (read-objs "(ns rksm.cloxp-repl-test.dummy-3)\n  (def x 23)\n")))))

(deftest eval-a-form
  (is (= 5
         (eval-form '(+ 3 2) *ns*))))

(deftest eval-a-def
  (let [expected-meta {:ns *ns*, :name 'x,
                       :file "NO_SOURCE_FILE", :column 17, :line 31
                       :test "123"}]
    (eval-form '(def x 23) *ns* {:add-meta {:test "123"}})
    (let [ref (find-var (symbol (str *ns*) "x"))]
      (is (= 23 (deref ref)))
      (is (= expected-meta (meta ref))))
    (eval-form '(def x 23) *ns* {:keep-meta [:line]})
    (let [ref (find-var (symbol (str *ns*) "x"))]
      (is (= 23 (deref ref)))
      (is (= (dissoc expected-meta :test) (meta ref))))))

(deftest eval-multiple-defs
  (eval-forms ['(def x 23) '(def y 24)] *ns*)
  (is (= 23 (-> (symbol (str *ns*) "x") find-var deref)))
  (is (= 24 (-> (symbol (str *ns*) "y") find-var deref)))
  )

(deftest eval-source-with-sym
  (eval-string "(def y 23)" *ns* {:line-offset 3})
  (is (= 23 (-> (symbol (str *ns*) "y") find-var deref)))
  (is (= {:ns *ns*,
          :name 'y,
          ; :file "rksm/cloxp_repl_test.clj",
          :line 4, :column 1,
          :end-column 7, :end-line 4
          :file "NO_SOURCE_FILE"
          :source "(def y 23)"
          :form '(def y 23)}
        (-> (symbol (str *ns*) "y") find-var meta))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (meta #'x)
 (test-var #'eval-a-def)
 (run-tests 'rksm.cloxp-repl-test)
    
 )