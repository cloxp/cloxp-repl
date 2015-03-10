(ns rksm.cloxp-repl
  (:require [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.string :as s]))

(def ^{:dynamic true,
       :doc "can be set by tooling to have larger code chunks accessible without
       special escaping"} *repl-source*)

(defn eval-expr
  [form ns & [{file :file}]]
  (binding [*ns* ns *file* file] (eval form)))

(defn eval-def
  [form ns & [{:keys [add-meta keep-meta] :as opts}]]
  (let [name (src-rdr/name-of-def form)
        sym (symbol (str ns) (str name))
        keep-meta (if keep-meta (some-> (find-var sym) meta (select-keys keep-meta)))
        m (merge add-meta keep-meta)]
    (let [new-def (eval-expr form ns opts)]
      (alter-meta! new-def merge m)
      new-def)))

(defn eval-form
  "possible keys in opts:
  :file :add-meta :keep-meta"
  [form ns & [opts]]

  (cond
    (src-rdr/def? form) (eval-def form ns opts)
    :default (eval-expr form ns opts)))

(defn eval-forms
  [forms ns & [opts]]
  (doall (map #(eval-form % ns opts) forms)))

(defn eval-string
  [string ns & [{:keys [line-offset] :or {line-offset 0} :as opts}]]
  (doall
    (for [read (src-rdr/read-objs string)]
      (let [line (partial + line-offset)
            evaled (eval-form (:form read) ns {assoc opts :add-meta read})]
        (if (src-rdr/def? (:form read))
          (alter-meta! evaled (comp #(update-in % [:line] line)
                                    #(update-in % [:end-line] line))))
        evaled))))
