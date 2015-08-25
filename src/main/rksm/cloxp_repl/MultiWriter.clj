(ns rksm.cloxp-repl.MultiWriter
  (:import (java.io Writer PrintStream PrintWriter StringWriter))
  (:gen-class :name rksm.cloxp-repl.MultiWriter
              :extends java.io.Writer
              :main false
              :init init
              :state writers
              :constructors {[java.io.Writer] []}
              :methods [[pushWriter [java.io.Writer] void]
                        [popWriter [] java.io.Writer]
                        [activeWriter [] java.io.Writer]]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; [Ljava.io.StringWriter;
; (class (into-array [(java.io.StringWriter.)]))
; #_[writers [] #^"[Ljava.io.StringWriter;"]

(defn -init
  [base-writer]
  [[] (atom [base-writer])])

(defn -pushWriter
  [this ^java.io.Writer writer]
  (swap! (.writers this) conj writer))

(defn -popWriter
  [this]
  (let [writer (last @(.writers this))]
    (swap! (.writers this) pop)
    writer))

(defn -activeWriter
  [this]
  (last @(.writers this)))

(defn -close
  [this]
  (.flush this)
  (doseq [w @(.writers this)] (.close w)))

(defn -write

  ([this x]
   (.write (-activeWriter this) x)
   (.flush ^Writer this))
  
  ([this x ^Integer off]
   (.write (-activeWriter this) x off)
   (.flush ^Writer this))
  
  ([this x ^Integer off ^Integer len]
   (.write (-activeWriter this) x off len)
   (.flush ^Writer this)))

(defn -flush
  [this]
  (.flush (-activeWriter this)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defmulti multi-writer? class)

(defmethod multi-writer? :default
  ([_] false))

(defmethod multi-writer? rksm.cloxp-repl.MultiWriter
  ([_] true))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment ; example
 (let [writer (rksm.cloxp-repl.MultiWriter.
               (java.io.PrintWriter. System/out))
       string-writer (StringWriter.)]
   (.pushWriter writer string-writer)
   (binding [*out* writer]
     ; will be printed to string writer
     (println "a")
     ; will be printed to system out
     (future (Thread/sleep 800) (println "b")))
   (.popWriter writer)
   (Thread/sleep 1000)
   (str string-writer)))
