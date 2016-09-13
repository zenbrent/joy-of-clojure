(ns chapter-6.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; A tree!
;; Each node has a value, and a left and right branch
;; mempty = nil
;; {:val 5 :L nil :R ni}

(defn xconj [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}
    (< v (:val t)) (assoc t :L (xconj (:L t) v))
    :else (assoc t :R (xconj (:R t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

;; A lot of fns in clojure are lazy:
; (and (println "a")
;      (println "b"))
;=> a
;=> nil

;; To make a function lazy:
;; Use the lazy-seq macro at the outermost level of the sequence producing expressions.
;; If using another sequence, use rest instead of next.
;; Prefer HOF over concrete
;; Don't hold on to your head.

(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []) )

(defn lz-rec-step [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lz-rec-step (rest s))]
      [])))

;; The book says these will print different things in the repl when they're defined, but they don't:
; (def very-lazy (-> (iterate #(do (print \.) (inc %)) 1)
;                    rest rest rest))
; (def less-lazy (-> (iterate #(do (print \.) (inc %)) 1)
;                    next next next))
;; ??

; (require '[clojure.core.async :as async :refer :all])

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(def tri-nums (map triangle (iterate inc 1)))

(nth tri-nums 99)
(take 10 (drop 99 tri-nums))


;; Explicit laziness:
(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive
  (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive)))

(defer-expensive
  (delay nil)
  (delay (do (Thread/sleep 5000) :expensive)))

;; This can be done similarly using functins: (delay expr) would be (fn [] expr) and
;; force would be (delayed-fn)
;; but you can check for delayed fns using delay?
;; delay caches to

(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

