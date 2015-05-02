(ns rksm.cloxp-repl.code-diff
  (:require [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.set :refer [difference union intersection]]
            [clojure.string :refer [trim]]))

(defn compare-src-pos
  [{l-a :line, c-a :column} {l-b :line, c-b :column}]
  (cond
    (< l-a l-b) -1
    (> l-a l-b) 1
    (< c-a c-b) -1
    (> c-a c-b) 1
    :default 0))

(defn- as-change
  [change]
  (if (or (= :modified change) (= :unmodified change))
    (fn [[old new]] {:change change :parsed-old old :parsed new})
    (fn [obj] {:change change :parsed obj})))

(defn- =src
  [a b]
  (= (-> a :source trim)
     (-> b :source trim)))

(defn- def-map
  [defs]
  (apply hash-map
    (mapcat (fn [{:keys [name defmethod-qualifier] :as obj}]
              [(str name "-" defmethod-qualifier) obj])
            defs)))

(defn- diff-defs
  [old-defs new-defs]
  (let [old-defs-map (def-map old-defs)
        new-defs-map (def-map new-defs)
        old-names (-> old-defs-map keys set)
        new-names (-> new-defs-map keys set)
        removed (difference old-names new-names)
        added (difference new-names old-names)
        maybe-changed (intersection new-names old-names)
        changed (remove #(=src (get old-defs-map %)
                               (get new-defs-map %))
                        maybe-changed)
        unchanged (difference maybe-changed (set changed))]
    {:removed (vals (select-keys old-defs-map removed))
     :added (vals (select-keys new-defs-map added))
     :modified (map vector
                    (vals (select-keys old-defs-map changed))
                    (vals (select-keys new-defs-map changed)))
     :unmodified (map vector
                      (vals (select-keys old-defs-map unchanged))
                      (vals (select-keys new-defs-map unchanged)))}))

(comment
 
 (def-map (src-rdr/read-objs "(defmulti multi-f x) (defmethod multi-f :a [_ x] (+ x 3))"))
 (def-map (src-rdr/read-objs "(def x 23)"))
 (diff-defs
  (src-rdr/read-objs "(defmulti multi-f x) (defmethod multi-f :a [_ x] (+ x 3))")
  (src-rdr/read-objs "(defmulti multi-f y) (defmethod multi-f :a [_ x] (+ x 3))")))

(defn- diff-defs-as-change
  [old-defs new-defs]
  (let [diffed (diff-defs old-defs new-defs)]
    (mapcat #(map (as-change %) (get diffed %)) (keys diffed))))

(defn- diff-toplevel-exps
  [old-exps new-exps]
  (loop [changes []
         [old-f & old-rest :as old] old-exps
         [new-f & new-rest :as new] new-exps]
    (cond
      (empty? old) (concat changes (map (as-change :added) new))
      (empty? new) (concat changes (map (as-change :removed) old))
      (=src old-f new-f) (recur
                           (concat [((as-change :unmodified) [old-f new-f])] changes)
                           (rest old) (rest new))
      :default
      (let [new-eq-to-old (first (filter (fn [ea] (=src old-f ea)) new))
            old-eq-to-new (first (filter (fn [ea] (=src new-f ea)) old))]
        (cond
          (and (nil? new-eq-to-old) (nil? old-eq-to-new)) (concat changes
                                                                  (map (as-change :removed) old)
                                                                  (map (as-change :added) new))
          new-eq-to-old (let [added (take-while (complement (partial =src old-f)) new)]
                          (recur
                            (concat changes (map (as-change :added) added))
                            old
                            (drop (count added) new)))
          old-eq-to-new (let [removed (take-while (complement (partial =src new-f)) old)]
                          (recur
                            (concat changes (map (as-change :removed) removed))
                            (drop (count removed) old)
                            new)))))))

(defn diff-read-objs
  [old-forms new-forms]
  (let [old-defs (filter (comp src-rdr/def? :form) old-forms)
        new-defs (filter (comp src-rdr/def? :form) new-forms)
        old-non-defs (remove (fn [f] (some #{f} old-defs)) old-forms)
        new-non-defs (remove (fn [f] (some #{f} new-defs)) new-forms)]
    (sort-by :parsed
             compare-src-pos
             (concat (diff-toplevel-exps old-non-defs new-non-defs)
                     (diff-defs-as-change old-defs new-defs)))))

(defn diff-source
  "outputs a list of changes in the form
  {:change      ; :modified OR :removed OR :added,
   :parsed      ; meta data result if src-rdr/read-objs when :change = :modified
   :parsed-old} ; when :change = :modified -- meta data result if src-rdr/read-objs
  changes are ordered by line/column source position taken from the item in :parsed"
  [old-src new-src]
  (diff-read-objs (src-rdr/read-objs old-src)
                  (src-rdr/read-objs new-src)))


(comment
 (diff-source "(def x 23)(def y 24)" "(def x 24)(def y 25)")
 )
