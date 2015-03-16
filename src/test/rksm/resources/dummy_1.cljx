(ns rksm.resources.test.dummy-1)

(defn x-to-string
  [x]
  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]
    (.append buf "x is: ")
    (.append buf (str x))))

(reify
  #+clj clojure.lang.IFn
  #+cljs cljs.core.IFn
  (invoke [_ x] (inc x)))