(ns rksm.cloxp-repl.nrepl
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.middleware :refer (set-descriptor!)]
            [clojure.tools.nrepl.middleware.interruptible-eval :as eval]
            [clojure.tools.nrepl.misc :refer (response-for)]
            [clojure.pprint :refer (pprint)])
  (:import clojure.tools.nrepl.transport.Transport
           (java.io PrintWriter)
           (rksm.cloxp-repl MultiWriter)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; printing values

(defmacro with-string-writer
  [sym body]
  `(let [~sym (java.io.StringWriter.)]
     ~body
     (str ~sym)))

(defn pretty-values
  "a replacement for clojure.tools.nrepl.middleware.pr-values/pr-values that can pretty print"
  [h]
  (fn [{:keys [op ^Transport transport pp pp-level] :as msg}]
    (h (assoc msg :transport
              (reify Transport
                (recv [this] (.recv transport))
                (recv [this timeout] (.recv transport timeout))
                (send [this resp]
                      (.send transport
                             (if-let [[_ v] (find resp :value)]
                               (assoc resp :value
                                      (with-string-writer w
                                        (cond
                                          pp (binding [*print-level* pp-level] (pprint v w))
                                          *print-dup* (print-dup v w)
                                          :default (print-method v w))))
                               resp))
                      this))))))


(set-descriptor! #'pretty-values
  {:requires #{}
   :expects #{}
   :handles {}})

(let [eval-descr
      (-> #'eval/interruptible-eval
        meta
        :clojure.tools.nrepl.middleware/descriptor
        (update-in [:requires] (fn [req]
                                 (-> req
                                   (disj #'clojure.tools.nrepl.middleware.pr-values/pr-values)
                                   (conj #'rksm.cloxp-repl.nrepl/pretty-values)))))]
  (set-descriptor! #'eval/interruptible-eval eval-descr))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; extended eval options

(def default-bindings {"clojure.core/*print-length*" nil
                       "clojure.core/*file*" nil})

(def ^:dynamic *cloxp-session?* false)
(def ^:dynamic *cloxp-out* nil)
(def ^:dynamic *cloxp-err* nil)

(defn prepare-eval
  [{:keys [op session bindings required-ns code transport] :as msg}]
  (if-not (get @session #'*cloxp-session?*)
    (swap! session assoc
           #'*cloxp-session?* true
           #'*cloxp-out* (MultiWriter. (PrintWriter. System/out))
           #'*cloxp-err* (MultiWriter. (PrintWriter. System/err))))
  (try
    (if required-ns
      (doseq [ns required-ns]
        (require (symbol ns))))
    (if bindings
      (doseq [[name expr] (merge default-bindings (apply hash-map bindings))]
        (swap! session assoc (find-var (symbol name)) (eval expr))))
    (catch Exception e
      (do
        (t/send transport
                (response-for msg
                              :status #{:error :done}
                              :error (str e)))
        (throw e))))
  (assoc msg
         :op "eval"
         :code code))

(defn wrap-cloxp-eval
  ""
  [h]
  (fn [{:keys [op] :as msg}]
    (if (not= op "cloxp-eval")
      (h msg)
      (h (prepare-eval msg)))))

(set-descriptor! #'wrap-cloxp-eval
  {:requires #{"clone" "close" #'pretty-values}
   :expects #{"eval"}
   :handles {"cloxp-eval"
             {:doc "Evaluation for the cloxp development environment. Allows to set additional options to change the evaluation environment."
              :requires {"code" "Source string of what should be evaluated."}
              :optional {"required-ns" "names of namespaces that needs to be loaded for evaluating `code`"
                         "bindings" "map, keys: names of dynamic vars, vals: expressions. thread-local bindings for evaluating `code`"}
              :returns (-> (meta #'eval/interruptible-eval)
                         ::clojure.tools.nrepl.middleware/descriptor
                         :handles
                         (get "eval")
                         :returns)}}})

