(ns rksm.cloxp-repl.code-diff-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-repl.code-diff :refer :all]))

(deftest diff-result
  (let [result (diff-source "(+ x 2)\n3" "3 4")
        expected [{:change :unmodified,
                   :parsed-old {:source "3\n", :form 3,
                                :end-column 1, :end-line 3, :line 2, :column 1},
                   :parsed {:source "3", :form 3,
                            :end-column 2, :end-line 1, :line 1, :column 1}} 
                  {:change :removed
                   :parsed {:source "(+ x 2)\n", :form '(+ x 2),
                            :end-column 1, :end-line 2, :line 1, :column 1}}
                  {:change :added
                   :parsed {:source "4\n", :form '4,
                            :end-column 1, :end-line 2, :line 1, :column 3}}]]
    (is (= expected result))))


(deftest simple-diff
  (let [test-data [["(def x 23)\n\n(+ x 2)\n3"
                    "(def x 23)\n\n(+ x 2)\n3"
                    [[:unmodified "(def x 23)\n"]
                     [:unmodified "(+ x 2)\n"]
                     [:unmodified "3\n"]]]
                  ["(def x 23)\n\n(+ x 2)\n3"
                    "(def x 24)\n\n(+ x 2)\n3"
                    [[:modified "(def x 24)\n"]
                     [:unmodified "(+ x 2)\n"]
                     [:unmodified "3\n"]]]
                   ["(def x 23)\n\n(+ x 2)\n3"
                    "(def x 23)\n2\n(+ x 2)"
                    [[:unmodified "(def x 23)\n"]
                     [:added "2\n"]
                     [:unmodified "(+ x 2)\n"]
                     [:removed "3\n"]]]
                   ["(def x 23)\n\n(+ x 2)\n3"
                    "(def x 24)\n2\n(+ x 2)"
                    [[:modified "(def x 24)\n"]
                     [:added "2\n"]
                     [:unmodified "(+ x 2)\n"]
                     [:removed "3\n"]]]]]
    
    (doseq [[old new expected] test-data]
      (let [result (map (juxt :change (comp :source :parsed)) (diff-source old new))]
        (is (= expected result))))))

(deftest def-diff-has-old-meta
  (is (= [["(def x 23)\n" "(def x 24)\n"]]
         (map (juxt (comp :source :parsed-old) (comp :source :parsed))
              (diff-source "(def x 23)" "(def x 24)")))))

(deftest keep-unmodified
  (is (= [[:unmodified "(def x 23)\n"]]
         (map (juxt :change (comp :source :parsed))
              (diff-source "(def x 23)" "(def x 23)")))))

(comment
 (run-tests *ns*)
 )