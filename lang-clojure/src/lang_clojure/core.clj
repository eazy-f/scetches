(ns lang-clojure.core)

(defrecord UniqAcc [seen orig])

(defn xform_uniq
  [reducer]
  (fn
    ([acc]
       (.orig acc))
    ([trans-acc n]
       (let [acc (if (instance? UniqAcc trans-acc)
                   trans-acc
                   (UniqAcc. #{} trans-acc))]
           (if (get (.seen acc) n)
             acc
             (UniqAcc. (conj (.seen acc) n)
                       (reducer (.orig acc) n)))))))

(defn transducer_uniq
  "use of stateful transducer"
  [& numbers]
  (transduce
   xform_uniq
   (completing conj)
   []
   numbers))
