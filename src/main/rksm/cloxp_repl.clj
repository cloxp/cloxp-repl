(ns rksm.cloxp-repl
  (:refer-clojure :exclude [load-file])
  (:require [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-repl.code-diff :as diff]
            [rksm.system-files :refer [file
                                       source-for-ns
                                       file-for-ns
                                       ns-name->rel-path]]
            [rksm.system-files.cljx :refer [cljx-file?]]
            [cljx.core :as cljx]
            [cljx.rules :as rules]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:import (clojure.lang Compiler)
           (java.io StringReader File)))

(def ^{:dynamic true,
       :doc "can be set by tooling to have larger code chunks accessible without
       special escaping"} *repl-source*)

(defn- ensure-ns
  [ns-or-sym]
  (cond
    (nil? ns-or-sym) *ns*
    (symbol? ns-or-sym) (find-ns ns-or-sym)
    :default ns-or-sym))

(defn eval-def
  [form ns & [{:keys [add-meta keep-meta] :as opts}]]
  (let [def? (src-rdr/def? form)
        name (if def? (src-rdr/name-of-def form))
        sym (symbol (str ns) (str name))
        keep-meta (if keep-meta
                    (some-> (find-var sym)
                      meta (select-keys keep-meta)))
        m (merge add-meta keep-meta)]
    (let [new-def (eval form)]
      (if (var? new-def) (alter-meta! new-def merge m))
      new-def)))

(defn eval-form
  "possible keys in opts: :file :add-meta :keep-meta.
  Returns a triple: [value error output]"
  [form ns & [{file :file, :or {file (or *file* "NO_SOURCE_FILE")} :as opts}]]
  (let [s (java.io.StringWriter.)
        [v e] (binding [*out* s
                        *file* (str file)
                        *ns* (ensure-ns ns)]
                (try
                  [(if (src-rdr/def? form)
                     (eval-def form ns opts)
                     (eval form))
                   nil]
                  (catch Exception e [nil e])))]
    [v e (str s)]))

(defn eval-read-obj
  "evaluates meta data objects returned by
  rksm.cloxp-source-reader.core/read-objs and returns a map of :parsed :value
  :out and :error."
  [{:keys [form name] :as parsed} ns
   & [{:keys [line-offset throw-errors?] :or {line-offset 0, throw-errors? false} :as opts}]]
  (let [[v e o] (eval-form form ns (update-in opts [:add-meta] merge parsed))]
    (if (and e throw-errors?) (throw e))
    (if (and (not e) v (not= 0 line-offset) (var? v))
      (alter-meta! v (comp #(update-in % [:line] + line-offset)
                           #(update-in % [:end-line] + line-offset))))
    {:parsed parsed :value v :error e :out o}))

(defn eval-forms
  [forms ns & [opts]]
  (doall (map #(eval-form % ns opts) forms)))

(defn eval-string
  "evaluates all toplevel expressions read from `string` and returns a map with
  :parsed :value :out and :error. :parsed is the result from
  rksm.cloxp-source-reader.core/read-objs"
  [string ns & [{:keys [file] :or {file (or *file* "NO_SOURCE_FILE")} :as opts}]]
  (let [cljx? (boolean (re-find #"\.cljx$" (str file)))]
    (binding [*ns* (ensure-ns ns) *file* (str file)]
      (->> (src-rdr/read-objs string {:cljx? cljx?})
       (map #(eval-read-obj % ns (merge opts {:file file})))
       doall))))

(defn- update-meta-from-read-obj
  [sym ns read-obj]
  (if-let [v (some-> (ns-interns ns) (get sym))]
    (let [pos (select-keys read-obj [:column :line :end-column :end-line])]
      (alter-meta! v merge pos))))

(defn- non-eval-result
  [read-obj old-read-obj prev-results]
  (let [find-prev-result
        (fn [{:keys [line column]}]
          (->> prev-results
            (filter #(= [line column] ((juxt :line :column) (:parsed %))))
            first))]
    (merge {:value nil :error nil :out ""}
           (find-prev-result old-read-obj)
           {:parsed read-obj})))

(defn eval-changed
  "given a list of prev-results maps (:parsed, :out, :error,:value), evaluate
  only those objs that have changed (according to
  rksm.cloxp-repl.code-diff/diff-read-objs). For unmodified objs return the
  previous eval result (out, error, value)"
  [objs prev-result ns & [{:keys [unmap-removed? eval-unchanged-exprs?]
                           :or {unmap-removed? true, eval-unchanged-exprs? true}
                           :as opts}]]
  (let [changed (diff/diff-read-objs (map :parsed prev-result) objs)]
    (->> changed
      (keep (fn [{:keys [parsed parsed-old change]}]
              (let [n (:name parsed)
                    def? (boolean n)]
                (case change
                  :unmodified (cond
                                def? (do
                                       ; if it's a def don't re-eval but update meta data
                                       (update-meta-from-read-obj n ns parsed)
                                       (non-eval-result parsed parsed-old prev-result))
                                (not eval-unchanged-exprs?) (non-eval-result parsed parsed-old prev-result)
                                :default (eval-read-obj parsed ns opts))
                  :modified (eval-read-obj parsed ns opts)
                  :added (eval-read-obj parsed ns opts)
                  :removed (do
                             (if unmap-removed?
                               (if-let [sym (:name parsed)]
                                 (ns-unmap ns sym)))
                             nil)))))
      doall)))

(defn eval-changed-from-source
  [source prev-source ns & [{:keys [file] :or {file (or *file* "NO_SOURCE_FILE")} :as opts}]]
  (binding [*ns* (ensure-ns ns) *file* (str file)]
    (let [objs (src-rdr/read-objs source)
          pseudo-prev-result (map (partial hash-map :parsed)
                                  (src-rdr/read-objs prev-source))]
      (eval-changed objs pseudo-prev-result ns opts))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn- compiler-load
  [source path & [fname]]
  (let [fname (or fname (-> path io/file .getName))]
    (binding [*file* path]
      (Compiler/load
       (StringReader. source)
       path fname))))

(defn load-file
  "Load-file equivalent"
  [source path & [{:keys [ns old-source cljx-rules] :as opts}]]
  (let [file-name (some-> path io/file .getName)
        ext (some->> file-name (re-find #"\.[^\.]+$"))
        cljx? (= ext ".cljx")
        x-rules (if cljx? (or cljx-rules rules/clj-rules))
        source (if cljx? (cljx/transform source x-rules) source)
        old-source (if (and old-source cljx?)
                     (cljx/transform old-source x-rules)
                     old-source)
        ns (if old-source
             (or ns
                 (src-rdr/read-ns-sym source)
                 'user))]
    (if old-source
      (some-> source
        (eval-changed-from-source old-source ns {:file path, :throw-errors? true})
        last :value)
      (compiler-load source path file-name))))

(defn require-ns
  [ns-sym & [file-name]]
  (try 
    (require ns-sym :reload)
    (catch java.io.FileNotFoundException e
      (if-let [file (file-for-ns ns-sym file-name #".cljx?$")]
         (let [ext (-> file .getName (re-find #"\.[^\.]+$"))
               relative-name (or file-name (ns-name->rel-path ns-name ext))]
           (load-file (slurp file) relative-name))
         (throw e)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (compiler-load "(+ 1 1) (+ 1 2)" "foo.clj")
 (load-file "(+ 1 1) (+ 1 2)" "foo.clj")
 (load-file
  "(+ 1 1) #+cljs (+ 1 2) #+clj (+ 1 3)" "foo.cljx"
  {:old-source "(+ 1 1) (+ 1 2)"})
 
 (let [prev-objs (src-rdr/read-objs "(+ 3 4) (def x 23) (+ 1 2)")
       new-objs (src-rdr/read-objs "(+ 3 4) (def x 23) (+ 1 3)")
       env (map (fn [r] {:parsed r :value 'old}) prev-objs)]
   (->> (eval-changed new-objs env 'user) (map :value)))
 ; => [7 'old 4]
 )