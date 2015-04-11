(ns rksm.cloxp-repl
  (:refer-clojure :exclude [load-file])
  (:require [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-repl.code-diff :as diff]
            [cljx.core :as cljx]
            [cljx.rules :as rules]
            [clojure.string :as s]))

(def ^{:dynamic true,
       :doc "can be set by tooling to have larger code chunks accessible without
       special escaping"} *repl-source*)

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
      (if def? (alter-meta! new-def merge m))
      new-def)))

(defn eval-form
  "possible keys in opts: :file :add-meta :keep-meta.
  Returns a triple: [value error output]"
  [form ns & [{file :file, :or {file "NO_SOURCE_FILE"} :as opts}]]
  (let [s (java.io.StringWriter.)
        [v e] (binding [*out* s
                        *file* file
                        *ns* (cond
                               (nil? ns) *ns*
                               (symbol? ns) (find-ns ns)
                               :default ns)]
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
   & [{:keys [line-offset] :or {line-offset 0} :as opts}]]
  (let [[v e o] (eval-form form ns (update-in opts [:add-meta] merge parsed))]
    (if (and (not e) v (not= 0 line-offset) (src-rdr/def? form))
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
  [string ns & [{:keys [file] :or {file "NO_SOURCE_FILE"} :as opts}]]
  (let [cljx? (boolean (re-find #"\.cljx$" (str file)))]
    (->> (src-rdr/read-objs string {:cljx? cljx?})
      (map #(eval-read-obj % ns (merge opts {:file file})))
      doall)))

(defn eval-changed
  "given a env map that has a list of prev-result maps (:parsed, :out, :error,
  :value), evaluate only those objs that have changed (according to
  rksm.cloxp-repl.code-diff/diff-read-objs). For unmodified objs return the
  previous eval result (out, error, value)"
  [objs prev-result ns & [{:keys [unmap-removed? eval-unchanged-exprs?]
                           :or {unmap-removed? true, eval-unchanged-exprs? true}
                           :as opts}]]
  (let [changed (diff/diff-read-objs (map :parsed prev-result) objs)
        find-prev-result (fn [{:keys [line column]}]
                           (->> prev-result
                             (filter #(= [line column] ((juxt :line :column) (:parsed %))))
                             first))]
    (->> changed
      (keep (fn [{:keys [parsed parsed-old change]}]
              (let [def? (-> parsed :name boolean)]
                (case change
                  :unmodified (if (or def? (not eval-unchanged-exprs?))
                                (merge {:value nil :error nil :out ""}
                                       (find-prev-result parsed-old)
                                       {:parsed parsed})
                                (eval-read-obj parsed ns opts))
                  :modified (eval-read-obj parsed ns opts)
                  :added (eval-read-obj parsed ns opts)
                  :removed (do
                             (if unmap-removed?
                               (if-let [sym (:name parsed)]
                                 (ns-unmap ns sym)))
                             nil)))))
      doall)))

(comment
 (let [prev-objs (src-rdr/read-objs "(+ 3 4) (def x 23) (+ 1 2)")
       new-objs (src-rdr/read-objs "(+ 3 4) (def x 23) (+ 1 3)")
       env (map (fn [r] {:parsed r :value 'old}) prev-objs)]
   (->> (eval-changed new-objs env 'user) (map :value)))
 ; => [7 'old 4]
 )
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn load-file
  "Load-file equivalent"
  [source source-path]
  (let [file-name (some-> source-path
                    (s/split (re-pattern (java.io.File/separator)))
                    last)
        ext (if source-path (str (re-find #"\.[^\.]+$" (str source-path))))
        cljx? (= ext ".cljx")
        source (if cljx? (cljx/transform source rules/clj-rules) source)]
    (eval
     (read-string
      (apply format
        "(clojure.lang.Compiler/load (java.io.StringReader. %s) %s %s)"
        (map (fn [item]
               (binding [*print-length* nil
                         *print-level* nil]
                 (pr-str item)))
             [source source-path file-name]))))))
