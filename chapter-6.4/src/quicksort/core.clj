(ns quicksort.core)

(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  ; (println {:work work})
  (lazy-seq
    (loop [[part & parts] work]
      ; (println {:x part :xs parts})
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list*
                   (filter smaller? xs)
                   pivot
                   (remove smaller? xs)
                   parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts))))))) 

(defn qsort [xs]
  (sort-parts (list xs)))

