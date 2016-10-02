(ns ch-10-mutation-concurrency.core
  (:import java.util.concurrent.Executors)
  (:use [ch-10-mutation-concurrency.dothreads :refer [dothreads!]]
        [midje.sweet]
        [clojure.pprint :refer [pprint]]
        [clojure.string :refer [join]]))

;; These are used to define identity, and determines change semantics:

;;;; Refs
;; Coordinated - multiple refs can be changed without race conditions.
;; Retriable -- changes are speculative and may have to be retried.
;;;; Agents
;; Async
;;;; Atoms
;; Retriable
;;;; Vars
;; Thread-local

;; All but vars are  considered shared references and allow changes to be seen
;; across threads of execution. Even though their use cases overlap, each
;; has an ideal use.

;; Concurrency is running multiple things at once
;; parallelism is partitioning work until multiple pieces that can run concurrently, typically towards building an aggregate.

;; "A faster program that doesn’t work right is useless." -- Peyton-Jones in Beautiful Concurrency

;; Time, state, identity:
;; relative moments that events occur
;; a snapshot of an entity at a moment in time
;; the logical entity identified by a common stream of states occuring over time.

;; Transaction is demarked by (dosync ...)
;; Changes in a dosync all succeed or fail together.

;; Deref all types with # or (deref ...)
;; They all mutate with different functions, but those all have the same structure.

;; 10.1.1 Making a mutable game board

(def initial-board
  [[:- :k :-]
   [:- :- :-] 
   [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %] (f s)))
            board)))

(defn board-print [board]
  (println (join "\n" 
                 (map #(->> %
                            (map name)
                            (join " "))
                      (board-map deref board)))))

(defn reset-board!
  "Resets the board state. Generally these types of functions are a bad idea."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]])) ;; describes the order of the moves
  (def num-moves (ref 0)))

(declare neighbors)

(def king-moves
  "Legal moves for the king"
  (partial neighbors
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move?
  "A move is legal when an enemy isn't there."
  [to enemy-sq]
  (when (not= to enemy-sq)
    to)) ;; nil if occupied

(defn choose-move
  "Randomly choose a legal move"
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

;; from 5.1
(def neighbors4
  [[-1 0] [1 0] [0 -1] [0 1]])

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

; (pprint (do (reset-board!)
;             (take 5 (repeatedly #(choose-move @to-move)))))

(defn place [from to] to)

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece) ;; get-in is awesome
  (alter (get-in board src) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

; (reset-board!)
; (doseq [_ (range 10000)] (make-move))
; (board-print board)

; (reset-board!)
; (dothreads! make-move :threads 100 :times 100)
; (board-print board)

;; Embedded transaction: clojure absorbs nested transactions into the parent,
;; so if one fails, they're all retried.

;; STM provides:
;; ACI -- atomicity, consistency, isolation, w/o manual locking.
;; in memory, so durability isn't relevant.
;; But:
;; Write skew -- in mvcc (multiversion concurrency control) happens when on transaction
;; uses a reference value to control it's behavoir, but doesn't write to it, and another
;; transaction writes to that value. Clojure can prevent this with `ensure`.
;; Live lock -- a set of transaction(s) that repeatedly restart each other. Clojure does
;; fight this.

;; In a transaction, don't:
;; IO -- because retries. Use io! macro when doing io to ensure it isn't used in a transaction.
;; e.g. (io! (.println System/out "sups yo!"))
;; (dosync (io! (.println System/out "sups yo!"))) ;; throws error
;; Mutate things -- b/c it's usu. not idempotent.



