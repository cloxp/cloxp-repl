(ns rksm.cloxp-repl.exception
  (:require [clj-stacktrace.repl :as strace]
            [rksm.cloxp-repl.util :refer [line-break-string]]))

(defn process-js-eval-exception
  [e]
  (let [{:keys [form js repl-env error]} (.getData e)
        {msg :value trace :stacktrace} error
        {{{:keys [host port]} :impl, :as s} :server
         client :client-id} repl-env
        server-name (-> s str)]
    {:printed
     (format
      "%s in client %s... connected to %s:\n[clj] %s\n\n[js] %s\n\n%s"
      msg (.substring client 0 5)
      server-name form js trace)}))

(defn process-default-clj-exception
  [e]
  (let [printed (clj-stacktrace.repl/pst-str e)
        lines (clojure.string/split-lines printed)]
    {:lines
     (map (fn [line]
            (let [[p? indent file line-no method] (re-find #"(\s*)([^:]+):([0-9]+) (.*)" line)
                  [_ ns fn] (if method (re-find #"([^\/]+)/([^\[]+)" method))
                  line-no (if line-no (read-string line-no))
                  java? (if file (re-find #"\.java$" file))]
              (cond-> {:string (line-break-string line)}
                p? (assoc :indent indent :file file :line line-no)
                (and p? java?) (assoc :java true :method method)
                (and p? ns) (assoc :clojure true :fn fn :ns ns))))
          lines)}))

(defn process-error
  "Parses a exception object, currently supports normal java.lang.Exceptions
  and processes them into a map of {:lines []} and cljs js-eval-exception infos
  which are processed to {:printed}"
  [e]
  (cond
    (and (instance? clojure.lang.ExceptionInfo e)
         (= (-> e .getData :type) :js-eval-exception)) (process-js-eval-exception e)
    (instance? java.lang.Throwable e) (process-default-clj-exception e)
    :default {:printed (str e)}))