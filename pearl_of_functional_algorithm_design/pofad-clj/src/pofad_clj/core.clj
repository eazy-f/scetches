(ns pofad-clj.core)

(defn merge-surpassers!
  [start middle end from to]
  [
   (first
    (reduce
     (fn [[tgt left-i right-i moved] tgt-i]
       (let [left  (first left-i)
             right (first right-i)
             from-val #(first (get from %1))]
         (let [[new-left-i new-rigth-i from-i new-moved]
               (cond
                (not left-i)
                  [left-i (next right-i) right moved]
                (not right-i)
                  [(next left-i) right-i left moved]
                (> (from-val left) (from-val right))
                  [(next left-i) right-i left moved]
                true
                  [left-i (next right-i) right (inc moved)])]
           (let [update-fn (if (< from-i middle)
                             #(+ moved %1)
                             identity)
                 src (update (get from from-i) 1 update-fn)]
             [(assoc! tgt tgt-i src) new-left-i new-rigth-i new-moved]))))
     [to (range start middle) (range middle end) 0]
     (range start end)))
   from])

(defn surpassers!
  [start end from to]
  (if (> (- end start) 1)
    (let [middle (int (/ (+ start end) 2))
          [t1 f1] (surpassers! start  middle to from)
          [t2 f2] (surpassers! middle end    t1 f1)]
      (merge-surpassers! start middle end t2 f2))
    [from to]))

(defn surpassers
  [coll]
  (let [coll-copy coll
        t1 (transient coll)
        t2 (transient coll-copy)]
    (->
     (surpassers! 0 (count coll) t1 t2)
     first
     persistent!)))

(defn line-print
  [something]
  (println something)
  something)

(defn max-surpasser
  "calculate maximum surpasser counter for a collection of comparable elements"
  [coll]
  (->>
   (for [x coll] [x 0])
   vec
   surpassers
   line-print
   (reduce #(max %1 (second %2)) 0)))

  
