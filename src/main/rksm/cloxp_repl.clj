(ns rksm.cloxp-repl
  (:refer-clojure :exclude [load-file])
  (:require [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-repl.code-diff :as diff]
            [rksm.cloxp-repl.MultiWriter :refer [multi-writer?]]
            [rksm.system-files :refer [file
                                       source-for-ns
                                       file-for-ns
                                       ns-name->rel-path
                                       namespaces-in-dir]]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.tools.namespace.parse :as parse]
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.reload :as reload])
  (:import (clojure.lang Compiler)
           (java.io StringReader File)))

(def ^{:dynamic true,
       :doc "can be set by tooling to have larger code chunks accessible without
       special escaping"} *repl-source*)

(def ^{:dynamic true,
       :doc "affects line in meta data when eval'ing defs"} *line-offset* 0)

(def ^{:dynamic true,
       :doc "affects column in meta data when eval'ing defs"} *column-offset* 0)

(defn- ensure-ns
  [ns-or-sym]
  (cond
    (nil? ns-or-sym) *ns*
    (symbol? ns-or-sym) (find-ns ns-or-sym)
    :default ns-or-sym))

(defn- read-string-with-features
  "via rksm.cloxp-source-reader.core/read-objs"
  [string ns {:keys [features file line-offset column-offset] :or {file (or *file* "NO_SOURCE_FILE")} :as opts}]
  (binding [*ns* (ensure-ns ns)
            *file* (str file)]
    (src-rdr/read-objs string {:features features
                               :line-offset (or line-offset *line-offset*)
                               :column-offset (or column-offset *column-offset*)})))

(defn read-clj-string
  "opts support :file :line-offset :column-offset :features. Returns a
  collection of parsed top-level expressions in the form of
  {:line NUMBER :column NUMBER
   :end-line NUMBER :end-column NUMBER
   :form SEQ :source STRING}"
  [string & [ns opts]]
  (let [ns (or ns 'user)
        opts (if (contains? opts :features)
               opts (assoc opts :features #{:clj}))]
    (read-string-with-features string ns opts)))

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

(defn- eval-defmulti
  [form ns & [opts]]
  (ns-unmap ns (src-rdr/name-of-def form))
  (eval-def form ns opts))

(defn- file-name
  "/foo/bar/baz.clj -> baz.clj"
  [^String file]
  (let [sep java.io.File/separator
        fn-re (re-pattern (format "%1$s([^%1$s]+)$" sep))
        [_ fn] (re-find fn-re file)]
    (or fn file)))

(defn eval-form
  "possible keys in opts: :file :add-meta :keep-meta.
  Returns a triple: [value error output]"
  [form ns & [{file :file, :or {file *file*}, :as opts}]]
  (let [string-writer (java.io.StringWriter.)
        writer (if (multi-writer? *out*)
                 (doto *out* (.pushWriter string-writer))
                 string-writer)
        file (let [f (str file)]
               (if (or (nil? f) (empty? f))
                 "NO_SOURCE_FILE" f))]
    (binding [*out* writer
              *file* file
              *source-path* (file-name file)
              *ns* (ensure-ns ns)]
      (conj
       (try
         [(cond
            (src-rdr/defmulti? form) (eval-defmulti form ns opts)
            (src-rdr/def? form) (eval-def form ns opts)
            :default (eval form))
          nil]
         (catch Exception e [nil e]))
       (do
         (when (multi-writer? writer) (.popWriter writer))
         (str string-writer))))))

(defn eval-read-obj
  "evaluates meta data objects returned by
  rksm.cloxp-source-reader.core/read-objs and returns a map of :parsed :value
  :out and :error."
  [{:keys [form name] :as parsed} ns
   & [{:keys [line-offset throw-errors?] :or {line-offset 0, throw-errors? false} :as opts}]]
  (let [add-to-meta (select-keys parsed [:line :column :source])
        [v e o] (eval-form form ns (update-in opts [:add-meta] merge add-to-meta))]
    (if (and e throw-errors?) (throw e))
    {:parsed parsed :value v :error e :out o}))

(defn eval-forms
  [forms ns & [opts]]
  (doall (map #(eval-form % ns opts) forms)))

(defn eval-string
  "evaluates all toplevel expressions read from `string` and returns a map with
  :parsed :value :out and :error. :parsed is the result from
  rksm.cloxp-source-reader.core/read-objs"
  [string ns & [{:keys [file line-offset column-offset] :or {file (or *file* "NO_SOURCE_FILE")} :as opts}]]
  (binding [*ns* (ensure-ns ns) *file* (str file)]
    (->> (read-clj-string string ns {:features #{:clj}
                                     :line-offset (or line-offset *line-offset*)
                                     :column-offset (or column-offset *column-offset*)})
      (map #(eval-read-obj % ns (merge opts {:file file})))
      doall)))

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
  previous eval result (out, error, value)
  Simply evaluating the top-level forms and defs that have changed is not
  always enough. For example, when a multi method declaration changes it and
  all defmethods need to be re-evaluated."
  [objs prev-result ns & [{:keys [unmap-removed? eval-unchanged-exprs?]
                           :or {unmap-removed? true, eval-unchanged-exprs? true}
                           :as opts}]]
  (let [changed (diff/diff-read-objs (map :parsed prev-result) objs)]
    (->> changed
      (reduce (fn [{:keys [results changed-defmulti] :as result}
                   {:keys [parsed parsed-old change]}]
                (let [n (:name parsed)
                      def? (boolean n)
                      form (:form parsed)
                      defmulti? (src-rdr/defmulti? form)
                      change (if (and (= :unmodified change)
                                      (src-rdr/defmethod? form)
                                      (contains? changed-defmulti n))
                               :modified change)
                      evaled (case change
                               :unmodified (cond
                                             def? (do
                                                    ; if it's a def don't re-eval but update meta data
                                                    (update-meta-from-read-obj n ns parsed)
                                                    (non-eval-result parsed parsed-old prev-result))
                                             (not eval-unchanged-exprs?) (non-eval-result parsed parsed-old prev-result)
                                             :default (eval-read-obj parsed ns opts))
                               :modified (do
                                           (if defmulti? (ns-unmap ns n))
                                           (eval-read-obj parsed ns opts))
                               :added (eval-read-obj parsed ns opts)
                               :removed (do
                                          (if unmap-removed?
                                            (if-let [sym n]
                                              (ns-unmap ns sym)))
                                          nil))]
                  (cond-> result
                    evaled (update-in [:results] conj evaled)
                    defmulti? (update-in [:changed-defmulti] conj n))))
              {:results [], :changed-defmulti #{}})
      :results doall)))

(defn eval-changed-from-source
  [source prev-source ns & [{:keys [file line-offset column-offset]
                             :or {file *file*} :as opts}]]
  (binding [*ns* (ensure-ns ns)
            *file* (let [f (str file)]
                     (if (or (nil? f) (empty? f))
                       "NO_SOURCE_FILE" f))]
    (let [objs (src-rdr/read-objs source {:features nil
                                          :line-offset (or line-offset *line-offset*)
                                          :column-offset (or column-offset *column-offset*)})
          pseudo-prev-result (map (partial hash-map :parsed)
                                  (read-clj-string prev-source *ns* {:features nil
                                                                     :line-offset (or line-offset *line-offset*)
                                                                     :column-offset (or column-offset *column-offset*)}))]
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
  [source path & [{:keys [ns old-source] :as opts}]]
  (let [file-name (some-> path io/file .getName)
        ext (some->> file-name (re-find #"\.[^\.]+$"))
        ns (if old-source
             (or ns
                 (src-rdr/read-ns-sym source)
                 'user))]
    (if old-source
      (some-> source
        (eval-changed-from-source old-source ns {:file path, :throw-errors? true})
        last :value)
      (compiler-load source path file-name))))

(defn load-ns-in-jar
  [ns-name ^java.io.File jar-file & [ext]]
  (let [path (rksm.system-files/ns-name->rel-path ns-name ext)
        file (rksm.system-files/file (str (.getPath jar-file) "!/" path))]
    (if (.exists file)
      (let [source (slurp file)]
        (load-file source path {:ns ns-name})))))

(defn require-ns
  [ns-sym & [file-name]]
  (try 
    (require ns-sym :reload)
    (catch java.io.FileNotFoundException e
      (if-let [file (file-for-ns ns-sym file-name #".cljc?$")]
         (let [ext (-> file .getName (re-find #"\.[^\.]+$"))
               relative-name (or file-name (ns-name->rel-path ns-name ext))]
           (load-file (slurp file) relative-name))
         (throw e)))))

(defn require-namespaces
  "re-loads(!) namespaces in dependency order"
  [ns-syms]
  (let [deps (->> ns-syms
               (map (fn [n]
                      (let [src (or (source-for-ns n) "")
                            decl (src-rdr/read-ns-decl src)
                            deps (parse/deps-from-ns-decl decl)]
                        {n deps})))
               (apply merge))
        tracker (track/add (track/tracker) deps)]
    (reload/track-reload tracker)))

(comment
 (clojure.tools.namespace.dependency/topo-sort
  (:clojure.tools.namespace.track/deps t2)))

(defn require-namespaces-in-dir
  [dir & [file-match]]
  (require-namespaces
   (namespaces-in-dir
    dir (or file-match #"\.cljc?$"))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (compiler-load "(+ 1 1) (+ 1 2)" "foo.clj")
 (load-file "(+ 1 1) (+ 1 2)" "foo.clj")
 
 (let [prev-objs (read-clj-string "(+ 3 4) (def x 23) (+ 1 2)")
       new-objs (read-clj-string "(+ 3 4) (def x 23) (+ 1 3)")
       env (map (fn [r] {:parsed r :value 'old}) prev-objs)]
   (->> (eval-changed new-objs env 'user) (map :value)))
 ; => [7 'old 4]
 )
