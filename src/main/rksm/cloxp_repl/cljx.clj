(ns rksm.cloxp-repl.cljx
  (:require [cljx.core :as cljx]
            [cljx.rules :as rules]
            [rksm.cloxp-repl.nrepl :as cloxp-nrepl]
            [clojure.tools.nrepl.middleware.load-file :as nrepl-load]
            [clojure.tools.nrepl.middleware.interruptible-eval :as eval]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; patching the nrepl load-file handler, somehwat similar to
; com.keminglabs/cljx.repl-middleware

(defn wrap-nrepl-load-file-for-cljx
  [h]
  (fn [{:keys [op file] :as msg}]
    (if (not= op "load-file")
      (h msg)
      (h (assoc msg :file (cljx/transform file rules/clj-rules))))))

(set-descriptor! #'wrap-nrepl-load-file-for-cljx
  {:requires #{"clone"}
   :expects #{#'nrepl-load/wrap-load-file}
   :handles {"load-file"
             {:doc "cljx transform"
              :requires {"file" "Full contents of a file of code."}
              :optional {"file-path" "Source-path-relative path of the source file, e.g. clojure/java/io.clj"
                         "file-name" "Name of source file, e.g. io.clj"}
              :returns (-> (meta #'eval/interruptible-eval)
                         ::clojure.tools.nrepl.middleware/descriptor
                         :handles
                         (get "eval")
                         :returns)}}})

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; patching nrepl eval

(defn wrap-nrepl-eval-for-cljx
  [h]
  (fn [{:keys [op code] :as msg}]
    (if (not= op "cloxp-eval")
      (h msg)
      (h (assoc msg :code (cljx/transform code rules/clj-rules))))))

(set-descriptor! #'wrap-nrepl-eval-for-cljx
  {:requires #{"clone"}
   :expects #{#'cloxp-nrepl/wrap-cloxp-eval}
   :handles {"cloxp-eval"
             {:doc "cljx transform"
              :requires {"code" "Source string of what should be evaluated."}
              :optional {"required-ns" "names of namespaces that needs to be loaded for evaluating `code`"
                         "bindings" "map, keys: names of dynamic vars, vals: expressions. thread-local bindings for evaluating `code`"}
              :returns (-> (meta #'eval/interruptible-eval)
                         ::clojure.tools.nrepl.middleware/descriptor
                         :handles
                         (get "eval")
                         :returns)}}})
