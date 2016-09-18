(ns a-star-pathfinding.core)

;; A* Pathfinding!
;; TODO: Read Dijkstra 1959 and play with more heuristics and worlds.

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

(def world2 [[  1   1   1   2  1]
             [  1   1   1 999  1]
             [  1   1   1 999  1]
             [  1   1   1 999  1]
             [  1   1   1   1  1]])

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

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (repeat size (vec (repeat size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                           (fn [w]
                             (let [[y x] w]
                               [(total-cost newcost step-est size y x) w]))
                           nbr-yxs))))))))) 


; (pprint (astar [0 0] 17 world))
; (pprint (astar [0 0] 900 world2))
; (pprint (astar [0 0] 900 (assoc-in world2 [4 3] 666)))
; (apply diff
;        (map #(-> % first  :yxs println)
;             [(astar [0 0] 900 world2)
;              (astar [0 0] 900 (assoc-in world2 [4 3] 666))]))
; (map println (assoc-in world2 [4 3] 666))

