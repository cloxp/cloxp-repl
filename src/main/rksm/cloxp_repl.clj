(ns rksm.cloxp-repl
  (:require [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as trt]
            [clojure.string :as s]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .rdr .source_log_frames var-get :buffer)
        str (.toString buf)]
    (.delete buf 0 (count str))
    str))

(defn read-objs
  [rdr-or-src]
  ; FIXME this is hacked...
  (let [rdr (trt/indexing-push-back-reader (trt/source-logging-push-back-reader rdr-or-src))]
    (loop [result []]
      (let [start-line (trt/get-line-number rdr)
            start-column (trt/get-column-number rdr)]
        (if-let [o (tr/read rdr false nil)]
          (let [raw-str (purge-string! rdr)
                lines (s/split-lines raw-str)
                no-ws-lines (take-while #(re-find #"^\s*(;.*)?$" %) lines)
                src-lines (drop (count no-ws-lines) lines)
                first-line-ws-match (re-matches #"^(\s*)(.*)" (first src-lines))
                src-lines (assoc (vec src-lines) 0 (nth first-line-ws-match 2))
                src (s/join "\n" src-lines)
                line (+ (count no-ws-lines) start-line)
                column (+ start-column (count (second first-line-ws-match)))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (recur (conj result {:form o
                                 :source src
                                 :line line
                                 :column column})))
          result)))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(defn unescape-slashes
  [src]
  (s/replace src (str "__" "SLASH" "__") "\\"))

(defn name-of-def
  [form]
  (first (drop 1 (filter symbol? form))))

(defn def?
  [form]
  (and (seq? form)
       (->> form first str (re-find #"(^|\/)def") boolean)))

(defn ns-name->rel-path
  [ns-name]
  (-> ns-name str
    (clojure.string/replace #"\." "/")
    (clojure.string/replace #"-" "_")
    (str ".clj")))

(defn eval-expr
  [form ns & [{file :file}]]
  (binding [*ns* ns *file* file] (eval form)))

(defn eval-def
  [form ns & [{:keys [add-meta keep-meta] :as opts}]]
  (let [name (name-of-def form)
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
    (def? form) (eval-def form ns opts)
    :default (eval-expr form ns opts)))

(defn eval-forms
  [forms ns & [opts]]
  (doall (map #(eval-form % ns opts) forms)))

(defn eval-string
  [string ns & [{:keys [line-offset] :or {line-offset 0} :as opts}]]
  (doall
    (for [read (read-objs (unescape-slashes string))]
      (let [+line (partial + line-offset)]
        (let [evaled (eval-form (:form read) ns {assoc opts :add-meta read})]
          (if (def? (:form read))
            (alter-meta! evaled (comp #(update-in % [:line] +line)
                                      #(update-in % [:end-line] +line))))
          evaled)))))
