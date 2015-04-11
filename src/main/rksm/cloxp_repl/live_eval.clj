(ns rksm.cloxp-repl.live-eval
  (:require [clojure.string :as s]
            [clojure.set :refer [difference union]]
            [rksm.cloxp-repl :refer [eval-changed]]
            [rksm.cloxp-source-reader.core :as src-rdr]))

(defn mapply [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn truncate
  [s len]
  (if (> (count s) len)
    (str (.substring s 0 len) "...")
    s))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defonce ^{:doc "cache used by live-eval-code-keeping-env"} envs (atom {}))

(defonce ^{:private true, :doc "Used by do-debounced"} reset-timeouts (ref {}))

(defn do-debounced
  "Will run func after timeout ms has passed, only if the do-debounced wasn't
  called inbetween."
  [id timeout func]
  (let [now (System/currentTimeMillis)
        stop-time (+ timeout now)
        last-entry (get @reset-timeouts id)]
    (if (or (not last-entry) (< last-entry stop-time))
      (dosync (alter reset-timeouts assoc id stop-time)))
    (if-not last-entry ; start thread
      (future
       (loop [time now stop-time stop-time]
         (let [sleep (- stop-time time)]
           (if (> sleep 0) (Thread/sleep sleep)))
         (let [now (System/currentTimeMillis)
               stop-time (get @reset-timeouts id)]
           (if (< now stop-time)
             (recur now stop-time)
             (do
               (dosync (alter reset-timeouts dissoc id))
               (func)))))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn- result-type
  [{:keys [value error]}]
  (class (or error value)))

(defmulti ^:private process-result result-type)

(defmethod process-result clojure.lang.Var
  [{var :value :as result}]
  (let [m (meta var)
        name (if (= *ns* (:ns m))
               (:name m)
               (str (:ns m) "/" (:name m)))
        val (deref var)]
    (assoc result :printed (str name " => " (truncate (str val) 20)))))

(defmethod process-result java.lang.Exception
  [{e :error :as result}]
  (assoc result :printed (pr-str e)))

(defmethod process-result :default
  [{x :value :as result}]
  (assoc result :printed (pr-str x)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; API

(defn live-eval-code
  "Takes source code and evaluates the top-level statements in it one-by-one.
  Will return a map {:results [{:value :pos :def? :source}], :defs {defnames -> result}}"
  [code & {:keys [prev-result ns] :or {prev-result []} :as opts}]
  (binding [*ns* (cond
                   (nil? ns) *ns*
                   (symbol? ns) (find-ns ns)
                   :default ns)]
    (let [result (eval-changed (src-rdr/read-objs code) prev-result ns opts)
         pretty-result (map process-result result)]
     pretty-result)))

(defn live-eval-code-keeping-env
  "Use this version of eval to have the live-eval state maintained for a
  certain amount of time (reset-timeout). After the timeout passed the state is
  cleared."
  [code & {:keys [id reset-timeout] :as opts}]
  {:pre [(not (nil? id))]
   :post [(contains? @envs id)]}
  (let [prev-result (get @envs id)
        eval-result (mapply live-eval-code
                            code (assoc opts :prev-result prev-result))]
    (swap! envs assoc id eval-result)
    (when reset-timeout
      (do-debounced
        id reset-timeout #(swap! envs dissoc id)))
    eval-result))
