(ns rksm.cloxp-repl.cljx
  (:require [cljx.core :as cljx]
            [cljx.rules :as rules]
            [clojure.tools.nrepl.middleware.load-file :as nrepl-load]
            [clojure.tools.nrepl.middleware.interruptible-eval :as eval]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; rk 2015-03-16:
; This is taken from cljx.repl-middleware, will require changes once features
; are supported in Clojure 1.7

(defn- find-resource
  [name]
  (if-let [cl (clojure.lang.RT/baseLoader)]
    (.getResource cl name)
    (ClassLoader/getSystemResourceAsStream name)))

; clojure.core/load from ~Clojure 1.6.0
; clojure.core/load hasn't really changed since ~2009, so monkey patching here
; seems entirely reasonable/safe.
(defn- cljx-load
  "Loads Clojure code from resources in classpath. A path is interpreted as
classpath-relative if it begins with a slash or relative to the root
directory for the current namespace otherwise."
  {:added "1.0"}
  [& paths]
  (doseq [^String path paths]
    (let [^String path (if (.startsWith path "/")
                          path
                          (str (#'clojure.core/root-directory (ns-name *ns*)) \/ path))]
      (when @#'clojure.core/*loading-verbosely*
        (printf "(clojure.core/load \"%s\")\n" path)
        (flush))
      (#'clojure.core/check-cyclic-dependency path)
      (when-not (= path (first @#'clojure.core/*pending-paths*))
        (with-bindings {#'clojure.core/*pending-paths* (conj @#'clojure.core/*pending-paths* path)}
          (let [base-resource-path (.substring path 1)
                cljx-path (str base-resource-path ".cljx")]
            (if-let [cljx (find-resource cljx-path)]
              (do
                (when @#'clojure.core/*loading-verbosely*
                  (printf "Transforming cljx => clj from %s.cljx\n" base-resource-path))
                (-> (slurp cljx)
                    (cljx/transform rules/clj-rules)
                    java.io.StringReader.
                    (clojure.lang.Compiler/load base-resource-path
                                                (last (re-find #"([^/]+$)" cljx-path)))))
              (clojure.lang.RT/load base-resource-path))))))))

(defonce ^:private clojure-load load)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; patching the nrepl load-file handler

(defonce ^:private nrepl-load-file-code nrepl-load/load-file-code)

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

(defn enable-cljx-load-support!
  []
  (alter-var-root #'load (constantly cljx-load)))

(defn disable-cljx-load-support!
  []
  (alter-var-root #'load (constantly clojure-load)))
