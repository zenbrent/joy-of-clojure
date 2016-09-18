(ns a-star-pathfinding.core)

;; A* Pathfinding!

;; Finds the least-difficult path to a goal.
;; The difficulty (cost) is found using a function (total-cost) that 
;; builds an estimate of the total cost from start point to a goal.
;; This function is used to sort candidate paths in the order most likely
;; to prove least costly.

(def world [[  1   1   1   1   1]  
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

;; from 5.1
(def neighbors4
  [[-1 0] [1 0] [0 -1] [0 1]])

(def neighbors8
  (for [l [[-1 0 1]]
        i l
        j l
        :when (not= 0 i j)]
    [i j]))


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

(defn estimate-cost
  "This function is often called g."
  [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2))) 

(defn path-cost [node-cost cheapest-nbr]
  "This is often called g, and is implemented as (+ (g ...) (h ...))"
  (+ node-cost
     (or (:cost cheapest-nbr) 0))) ;; <-- Add cheapest neighbor cost, else 0.

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

;; Good for finding the cheapest path by cost heuristic.
(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other)) ;; Successively bubble the min value out to the return
                other
                min))
            coll)))



