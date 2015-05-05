(ns rksm.cloxp-repl-test
  (:refer-clojure :exclude [load-file])
  (:require [clojure.test :refer :all]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-repl :refer :all]))

(defonce this-file (-> *ns* 
                     ns-name munge
                     (clojure.string/replace #"\." "/")
                     (str ".clj")
                     clojure.java.io/resource .getFile))

(defmacro codify
  [& body]
  `(clojure.string/join "\n" (map str '~body)))

(defmacro lookup-var
  [var-sym]
  `(let [ns-name# (or (namespace '~var-sym) (str *ns*))
         var# (-> (symbol ns-name# (str (name '~var-sym))) find-var)]
     (if (and var# (bound? var#)) var# nil))) 

(defmacro lookup
  [var-sym]
  `(some-> (lookup-var ~var-sym) deref)) 

(defn fixture [test]
  (test)
  (ns-unmap *ns* 'x)
  (ns-unmap *ns* 'y))

(use-fixtures :each fixture)

(deftest eval-a-form
  (is (= [5 nil ""]
         (eval-form '(+ 3 2) *ns*))))

(deftest eval-a-def
  (binding [*file* this-file]
    (let [expected-meta {:ns *ns*, :name 'x,
                         :file this-file,
                         :column 19, :line 44
                         :test "123"}]
      (eval-form '(def x 23) *ns* {:add-meta {:test "123"}})
      (is (= 23 (lookup x)))
      (is (= expected-meta (-> x lookup-var meta (dissoc :end-line :end-column))))
      (eval-form '(def x 23) *ns* {:keep-meta [:line :end-line]})
      (is (= 23 (lookup x)))
      (is (= (dissoc expected-meta :test) (-> x lookup-var meta (dissoc :end-line :end-column)))))))

(deftest eval-sets-source-pos
  (binding [*file* this-file]
    (let [expected-meta {:ns *ns*, :name 'x,
                         :file this-file,
                         :column 19, :line 44
                         :test "123"}]
      (eval-string (codify (def x 23)) *ns* {:line-offset 2 :column-offset 3})
      (is (= [3 4] (-> x lookup-var meta ((juxt :line :column))))))))

(deftest eval-multiple-defs
  (eval-forms ['(def x 23) '(def y 24)] *ns*)
  (is (= 23 (lookup x)))
  (is (= 24 (lookup y))))

(deftest eval-source-with-sym
  (binding [*file* this-file]
    (let [[{:keys [value error]}] (eval-string "(def y 23)" *ns* {:line-offset 3})]
      (is (var? value))
      (is (nil? error))))
  (is (= 23 (lookup y)))
  (is (= {:ns *ns*,
          :name 'y,
          :line 4, :column 1,
          :file this-file
          :source "(def y 23)\n"}
         (-> y lookup-var meta (dissoc :end-line :end-column)))))

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
               :value (lookup-var y)
               :error nil :out ""}]
             result))
      (is (nil? (lookup-var x)))))
  
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

(deftest eval-changed-from-source-test
  (testing "only eval changed top level forms"
    (let [old-code "(def x 23) (def y 24) (+ 3 4) (+ 1 2)"
          new-code "(def x 23) (def y 25) (+ 3 4) (+ 1 3)"
          result (eval-changed-from-source new-code old-code *ns*)
          vals (map :value result)
          expected-vals [nil
                         (lookup-var y)
                         7
                         4]]
      (is (= expected-vals vals))
      (is (nil? (lookup x)))
      (is (= 25 (lookup y))))))

(deftest update-mets-of-unchanged-defs
  
  (testing "only eval changed top level forms"
    (let [code-1 ""
          code-2 "(def x 23) (def y 24)"
          code-3 "(def x 23) \n(def y 24)"]
      (eval-changed-from-source code-2 code-1 *ns*)
      (eval-changed-from-source code-3 code-2 *ns*)
      (is (= [2 1] (-> (lookup-var y) meta ((juxt :line :column))))))))

(deftest eval-changed-updates-multimethods

  (testing "modifying defmethod"
    (remove-ns 'multi-test-1)
    (let [f (java.io.File/createTempFile "multi_test_1" ".clj")
          source-1 (codify (ns multi-test-1)
                           (defmulti multi-f (fn [x & _] x))
                           (defmethod multi-f :a [_ x] (+ x 2)))
          source-2 (codify (ns multi-test-1)
                           (defmulti multi-f (fn [x & _] x))
                           (defmethod multi-f :a [_ x] (+ x 3)))
          _ (do (spit f source-1) (clojure.core/load-file (str f)))
          result (eval-changed-from-source source-2 source-1 'multi-test-1)]
      (is (= 6 ((lookup multi-test-1/multi-f) :a 3)))))

  (testing "modifying defmulti"
    (remove-ns 'multi-test-2)
    (let [f (java.io.File/createTempFile "multi_test_2" ".clj")
          source-1 (codify (ns multi-test-2)
                           (defmulti multi-f (fn [x & _] x))
                           (defmethod multi-f :a [_ x] (+ x 2))
                           (defmethod multi-f :b [_ x] (+ x 3)))
          source-2 (codify (ns multi-test-2)
                           (defmulti multi-f (constantly :b))
                           (defmethod multi-f :a [_ x] (+ x 2))
                           (defmethod multi-f :b [_ x] (+ x 3)))
          _ (do (spit f source-1) (clojure.core/load-file (str f)))
          result (eval-changed-from-source source-2 source-1 'multi-test-2)]
      (is (= 6 ((lookup multi-test-2/multi-f) :a 3))))))

(deftest eval-string-updates-multimethods

  (testing "modifying defmulti"
    (remove-ns 'multi-test-2)
    (let [f (java.io.File/createTempFile "multi_test_2" ".clj")
          source-1 (codify (ns multi-test-2)
                           (defmulti multi-f (fn [x & _] x))
                           (defmethod multi-f :a [_ x] (+ x 2))
                           (defmethod multi-f :b [_ x] (+ x 3)))
          source-2 (codify (ns multi-test-2)
                           (defmulti multi-f (constantly :b))
                           (defmethod multi-f :a [_ x] (+ x 2))
                           (defmethod multi-f :b [_ x] (+ x 3)))
          _ (do (spit f source-1) (clojure.core/load-file (str f)))
          _ (eval-string source-2 'multi-test-2)]
      (is (= 6 ((lookup multi-test-2/multi-f) :a 3))))))
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (let [w (java.io.StringWriter.)]
   (binding [*test-out* w]
    (run-tests *ns*) w))
 )