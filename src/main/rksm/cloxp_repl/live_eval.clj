(ns rksm.cloxp-repl.live-eval
  (:require [clojure.string :as s]
            [clojure.set :refer [difference union]]
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
  [{:keys [form line column source]}]
  (let [s (java.io.StringWriter.)
        value (process-result
               (binding [*out* s]
                 (try (eval form) (catch Exception e e))))
        out (str s)]
    {:pos {:line line :column column},
     :value value, :out out, :source source,
     :form form, :def? (src-rdr/def? form)}))

(defn prev-def-result
  [{form :form, :as expr} {:keys [defs], :as env}]
  (get defs (src-rdr/name-of-def form)))

(defn changed?
  [{new-src :source, :as expr} env]
  (let [{old-source :source} (prev-def-result expr env)]
    (not= new-src old-source)))

(defn make-env
  [eval-result]
  {:results (map #(dissoc % :form) eval-result)
   :defs (apply hash-map
           (->> eval-result
             (filter :def?)
             (mapcat (juxt (comp src-rdr/name-of-def :form) identity))))})

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; API

(defn live-eval-code
  "Takes source code and evaluates the top-level statements in it one-by-one.
  Will return a map {:results [{:value :pos :def? :source}], :defs {defnames -> result}}"
  [code & {:keys [file ns env], :as opts}]
  (binding [*ns* (if ns (find-ns ns) *ns*)
            *file* file]
    (let [eval-result
          (map (fn [{:keys [form line column] :as expr}]
                 (if (or (not (src-rdr/def? form)) (changed? expr env))
                   (eval-code expr)
                   (assoc (prev-def-result expr env)
                          :pos {:line line :column column})))
               (src-rdr/read-objs code))
          changed-env (make-env eval-result)]
      changed-env)))

(defn live-eval-code-with-changes
  "Will compare source of defs in env with defs that are defined in code. Will
  only eval those defs for which the source has changed. This avoids
  unnecessarily re-defining defs and the side effects this has."
  [code & {:keys [env ns] :or {ns 'user} :as opts}]
  (let [{old-defs :defs} env
        {defs :defs, :as eval-result} (mapply live-eval-code code opts)
        added (difference (set (keys defs)) (set (keys old-defs)))
        removed (difference (set (keys old-defs)) (set (keys defs)))
        changed (set (filter
                      (fn [def-name]
                        (let [get-val (fn [defs] (-> defs (get def-name) :value))]
                          (not= (get-val defs) (get-val old-defs))))
                      (difference (set (keys defs)) added)))
        changes {:added added, :removed removed, :changed changed}]
    (doseq [def-name removed] (ns-unmap ns def-name))
    (assoc eval-result :changes changes)))

(defn live-eval-code-keeping-env
  "Use this version of eval to have the live-eval state maintained for a
  certain amount of time (reset-timeout). After the timeout passed the state is
  cleared."
  [code & {:keys [id reset-timeout] :as opts}]
  {:pre [(not (nil? id))]
   :post [(contains? @envs id)]}
  (let [prev-env (get @envs id)
        eval-result (mapply
                     live-eval-code-with-changes
                     code (assoc opts :env prev-env))]
    (swap! envs assoc id eval-result)
    (when reset-timeout
      (do-debounced
       id reset-timeout #(swap! envs dissoc id)))
    eval-result))
