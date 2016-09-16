(ns unit-calc.core)

(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})

(def simple-data {:bit 1
                   :byte 8
                   :nibble [1/2 :byte]})

(defn convert
  [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))
