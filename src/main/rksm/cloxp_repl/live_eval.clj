(ns rksm.cloxp-repl.live-eval
  (:require [clojure.string :as s]
            [rksm.cloxp-source-reader.core :as src-rdr]))

(defn truncate
  [s len]
  (if (> (count s) len)
    (str (.substring s 0 len) "...")
    s))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defmulti process-result class)

(defmethod process-result clojure.lang.Var
  [var]
  (let [m (meta var)
        name (if (= *ns* (:ns m))
               (:name m)
               (s/join "/" ((juxt (comp str :ns) (constantly "/") (comp str :name)) m)))
        val (deref var)]
    (str name " => " (truncate (str val) 20))))

(defmethod process-result java.lang.Exception
  [e]
  (pr-str e))

(defmethod process-result :default
  [x]
  (pr-str x))

(defn eval-code
  [{:keys [form line column]}]
  (let [s (java.io.StringWriter.)
        value (process-result
               (binding [*out* s]
                 (try (eval form) (catch Exception e e))))
        out (str s)]
    {:pos {:line line :column column}, :value value, :out out}))

(defn live-eval-code
  [code & {:keys [file ns], :as opts}]
  (binding [*ns* (if ns (find-ns ns) *ns*) *file* file]
    (->> (src-rdr/read-objs code)
      (map eval-code)
      doall)))
