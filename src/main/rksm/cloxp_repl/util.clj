(ns rksm.cloxp-repl.util
  (:require [clojure.string :refer [split join]]))

(defn line-break-string
  "lines-breaks a string to make it readable"
  [string & [{:keys [max-line-length] :or {max-line-length 80}}]]
  (let [lines (loop [[word & rest] (split string #"\s")
                     lines []
                     length 0]
                (if (nil? word)
                  lines
                  (let [line-length (+ (count word) length)]
                    (if (> line-length max-line-length)
                      (recur rest (conj lines word) 0)
                      (let [last-line (or (peek lines) "")
                            lines (conj (if (empty? lines) [] (pop lines))
                                        (if (empty? last-line) word (str last-line " " word)))]
                        (recur rest lines line-length))))))]
    (join "\n" lines)))