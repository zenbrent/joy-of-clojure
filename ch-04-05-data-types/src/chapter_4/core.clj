(ns chapter-4.core
  (:gen-class)
  (:require clojure.set))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; re-seq returns a lazy seq of all matches
(re-seq #"\w+" "one-two/three") ;=> ("one" "two" "three")
(re-seq #"\w*(\w)" "one-two/three") ;=> (["one" "e"] ["two" "o"] ["three" "e"])

;; add a bunch of items to a list:
(into [1 2 3] (range 10))
; to create a vector can use (vector ...items)
;; Can create vectors of a specific type:
(into (vector-of :int) [Math/PI 123 1.24]) ;=> [3 123 1]
(into (vector-of :char) [100 101 102])
; (into (vector-of :int) [1 2 623876371267813267326786327863])
;; it coerses any values inserted to that type.

;; nth, get, and vector as a function all behave differently

; (rseq ) is (seq ) but backwards.

;; these are good for nested structures:
; (assoc-in )
; (get-in matrix [1 2]) ;; for something like [[...] [...] [...]]

(def neighbors4
  [[-1 0] [1 0] [0 -1] [0 1]])

(def neighbors8
  (for [l [[-1 0 1]]
        i l
        j l
        :when (not= 0 i j)]
    [i j]))

(defn get-matrix
  [x y]
  (vec (for [x (range x)]
    (vec (for [y (range y)]
      [x y])))))

(defn neighbors
  "Returns a vector of [x y] pairs for neighboring cells on a grid."
  ([size yx]
   (neighbors neighbors4
              size
              yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

(let [size 11
      mx (get-matrix size size)]
  (map #(get-in mx %) (neighbors neighbors8 size [6 5])))

;; If an algorithm uses a vector as a stack, use
;; conj, peek, and pop for performance and semantic reasons.

;; (subvec vec start-inclusive end-exclusive)
;; subvec returns a vector with an index mapping from the original to the new vector.
;; it flattens changes, so (subvec (subvec vec ...)) will return the original vec with
;; all needed mappings.

;; Iterating through a map gives you [:key value] vectors.

;; Vectors aren't sparse:
;; For sorted collection indexed by non-sequential #s, use a hash or sorted map.

;; PersistentQueue immutable structure
;; conj adds to end
;; peek = first
;; pop = rest -- except rest returns a seq, (pop queue) returns a queue.
;; if you need a work queue, use derivatives of java.util.concurrent.BlockingQueue
;; PersistentQueue is immutable, so it doesn't inherently communicate changes from one worker to another.

;; print a queue fish.
(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth))

; (peek schedule)
; (pop schedule)

;; sets
; (into #{#{} {}} [()])
; (into #{#{} {} []} [()])
;; both return #{() #{} {}}
;; because () and [] are both in the same equality partition.

; (some map vec)
;; e.g.
; (some #{1 2} [1 2 3 4])
;; uses #{1 2} as the predicate, returning nil if a value isn't in the set.

;; Sorted sets must be of comparable types, e.g.:
; (sorted-set 1 2 3 :a :b :c)
;; throws a type cast error.
; This can be sometimes alleviated by using (sorted-set-by ..) which takes a comparator.

(contains? #{1 2 3 4} 4)
(contains? [1 2 3 4] 4)
;; contains? operates on the keys of a collection. sets use the values as both keys & values.

;; clojure.set namespace
; intersection
; union
; difference
(clojure.set/difference #{1 2 3 4} #{3 4 5 6})
;=> #{1 2} ;; relative complement (Stewart 1995), removes all elements in set a that are also in set b.

(= (hash-map :1 1 :2 2 :3 3)
   {:1 1 :2 2 :3 3})
;=> true

(= (into {} [[:a 1] [:b 2]])
   (apply hash-map [:a 1 :b 2])
   (zipmap [:a :b] (range 1 3))
   {:a 1 :b 2})

; (sorted-map map) ;; sorts a map by it's keys
; (sorted-map-by ..)

(sorted-map-by #(compare (subs %1 1) (subs %2 1)) "bac" 2 "abc" 9)
(sorted-map-by < 0 :0 5 :5 1 :1 2 :2)

; Sorted maps & sets are very fast at jumping to a key and walking forward or backward using subseq and rsubseq.
; Even if you don't know the exact key you want, this lets you "round up" to the next closest, existing key

(assoc {1 :int} 1.0 :float)
(assoc (sorted-map 1 :int) 1.0 :float)
;; Sorted maps treat different types of numbers with the same magnitude as the same.
;; Keys are sorted with the comparator, and if they are equal, only one is kept.

;; Array maps gurantee insertion ordering!
(seq (hash-map :a 1, :b 2, :c 3))
(seq (array-map :a 1, :b 2, :c 3))
;; If ordering is important, be sure to not do operations that will change the underlying
;; map implementation.

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(def alphabet
  (map char (range 97 (+ 26 97))))

(pos #{\b \r \e \n \t} alphabet)

