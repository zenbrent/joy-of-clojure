(ns ch-14-data-orientation.core
  (:require [clojure.data :as data]
            [clojure.data.generators :as gen]
            [clojure.test.generative :refer (defspec)]
            [clojure.edn :as edn]))

;; 14.12.1 The benefits of value

;; Values can be reproduced.
;; edn

;; And fabricated.
(def ascii (map char  (range 65 (+ 65 26))))

(defn rand-str [sz alphabet]
  (apply str (repeatedly sz #(rand-nth alphabet))))

(rand-str 10 ascii)

(def rand-sym #(symbol (rand-str %1 %2)))
(def rand-key #(keyword (rand-str %1 %2)))

(map #(% 10 ascii)
     [rand-str rand-sym rand-key])

(defn rand-vec [& generators]
  (into [] (map #(%) generators)))

(rand-vec #(rand-sym 5  ascii)
          #(rand-key 10 ascii)
          #(rand-int 1024))

(defn rand-map [sz kgen vgen]
  (into {}
        (repeatedly sz #(rand-vec kgen vgen))))

(rand-map 3 #(rand-key 5 ascii) #(rand-int 100))
(rand-map 5 #(rand-key 10 ascii) #(rand-int 1024))

;; Values facilitate testing

;; diffs are easy:
(let [[left right both] (data/diff [1 2 3] [1 2 4])]
  {:left left :right right :both both})

(defn slope
  "From 7.1.15"
  [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(defspec slope-rules
  (fn [p1 p2] (slope :p1 p1 :p2 p2))
  [^{:tag (vec long 2)} p2, ^{:tag (vec long 2)} p2]
  (assert (float? %)))


;; Other benefits...


;; Tagged literals rock 343
;; Generic syntax, deferred type.

;; #a-ns/tag some-legal-clj-form

;; e.g.
#inst "1969-08-18"
#uuid "eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee"

;; How to make #unit/length [1 :km]

(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})

(def simple-data {:bit 1
                  :byte 8
                  :nibble [1/2 :byte]})

(defn convert
  "From ch-07.3.1-unit-calc"
  [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(def distance-reader
  (partial convert simple-metric))

#unit/length [1 :km]

(def time-reader
  (partial convert
           {:sec 1
            :min 60,
            :hr  [60 :min],
            :day [24 :hr]}))

;; Or manually bind readers:

(binding [*data-readers* {'unit/time ch-14-data-orientation.core/time-reader}]
  (read-string "#unit/time [1 :min 30 :sec]"))

;; A default tag for readers that don't exist:
(binding [*default-data-reader-fn* #(-> {:tag %1 :payload %2})]
  (read-string "#nope [:doesnt-exist]"))

;; Don't use the reader to process untrusted data! It's code!

(edn/read-string "#uuid \"dae78a90-d491-11e2-8b8b-0800200c9a66\"")
(edn/read-string "{:a 42, \"b\" 36, [:c] 9}")

; (edn/read-string "#unit/time [1 :min 30 :sec]")
;; errors --^ ;; because it doesn't know which readers are available.

(def T {'unit/time ch-14-data-orientation.core/time-reader})

(edn/read-string {:readers T} "#unit/time [1 :min 30 :sec]")

(edn/read-string {:readers T, :default vector} "#wt/f :???")


