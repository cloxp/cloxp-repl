(ns rksm.cloxp-repl-test
  (:refer-clojure :exclude [load-file])
  (:require [clojure.test :refer :all]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-repl :refer :all]))

(defn fixture [test]
  (test)
  (ns-unmap *ns* 'x)
  (ns-unmap *ns* 'y))

(use-fixtures :each fixture)

(deftest eval-a-form
  (is (= [5 nil ""]
         (eval-form '(+ 3 2) *ns*))))

(deftest eval-a-def
  (let [expected-meta {:ns *ns*, :name 'x,
                       :file "NO_SOURCE_FILE", :column 17, :line 22
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
  (is (= 24 (-> (symbol (str *ns*) "y") find-var deref))))

(deftest eval-source-with-sym
  (let [[{:keys [value error]}] (eval-string "(def y 23)" *ns* {:line-offset 3})]
    (is (var? value))
    (is (nil? error)))
  (is (= 23 (-> (symbol (str *ns*) "y") find-var deref)))
  (is (= {:ns *ns*,
          :name 'y,
          ; :file "rksm/cloxp_repl_test.clj",
          :line 4, :column 1,
          :end-column 1, :end-line 5
          :file "NO_SOURCE_FILE"
          :source "(def y 23)\n"
          :form '(def y 23)}
        (-> (symbol (str *ns*) "y") find-var meta))))

(deftest eval-changed-test
  
  (testing "don't re-eval form"
    (let [prev-objs (src-rdr/read-objs "(def x 23) (def y 24)")
          new-objs (src-rdr/read-objs "(def x 23) (def y 25)")
          prev-result (map (fn [r] {:parsed r :value 'old}) prev-objs)
          result (eval-changed new-objs prev-result *ns*)]
      (is (= [{:parsed (first new-objs)
               :value 'old
               :error nil :out ""}
              {:parsed (second new-objs)
               :value (-> (symbol (str *ns*) "y") find-var)
               :error nil :out ""}]
             result))
      (is (nil? (-> (symbol (str *ns*) "x") find-var)))))
  
  (testing "eval unchanged non-defs"
    (let [prev-objs (src-rdr/read-objs "(+ 3 4)")
          new-objs (src-rdr/read-objs "(+ 3 4)")
          prev-result (map (fn [r] {:parsed r :value 'old}) prev-objs)
          result (map :value (eval-changed new-objs prev-result *ns*))]
      (is (= [7] result))))
  
  (testing "explicitly not eval unchanged non-defs"
    (let [prev-objs (src-rdr/read-objs "(+ 3 4)")
          new-objs (src-rdr/read-objs "(+ 3 4)")
          prev-result (map (fn [r] {:parsed r :value 'old}) prev-objs)
          result (map :value (eval-changed new-objs prev-result *ns* {:eval-unchanged-exprs? false}))]
      (is (= ['old] result))))
  
  (testing "removed expressions are ignored"
    (let [prev-objs (src-rdr/read-objs "(+ 1 2)")
          new-objs (src-rdr/read-objs "(+ 1 3)")
          prev-result (map (fn [r] {:parsed r :value 'old}) prev-objs)
          result (map :value (eval-changed new-objs prev-result *ns*))]
      (is (= [4] result))))
  
  (testing "removed defs are unmapped"
    (def x 23)
    (let [prev-objs (src-rdr/read-objs "(def x 23) (def y 24)")
          new-objs (src-rdr/read-objs "(def y 24)")
          prev-result (map (fn [r] {:parsed r :value 'old}) prev-objs)
          _ (map :value (eval-changed new-objs prev-result *ns*))]
      (is (nil? (-> (symbol (str *ns*) "x") find-var))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests 'rksm.cloxp-repl-test)
 )