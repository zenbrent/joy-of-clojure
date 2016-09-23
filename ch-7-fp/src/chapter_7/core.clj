(ns chapter-7.core
  (:require [clojure.test :as t]))

(comment Functions!)

;; Data acts as functions LOL
(map [:a :b :c :d] #{0 2})

;; Functions as data

(def fifth (comp first rest rest rest rest))

(fifth [1 2 2 3 4])

;; Thinking functionally:
(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

((fnth 5) '[a b c d e])

;; Thinking compositionally:

(map (comp
       keyword
       #(.toLowerCase %)
       name)
     '(a b c))

((partial + 5) 100 200)
(#(apply + 5 %&) 100 200) ;; <---------   LOOK AT THIS!!!! Varardic compact functions with %&

(let [truthiness (fn [v] v)]
  [((complement truthiness) true) ;; false
   ((complement truthiness) 42) ;; false
   ((complement truthiness) false) ;; true
   ((complement truthiness) nil)]) ;; true

((complement even?) 2)
;; equivalent to:
; (comp not even?)
; #(not (even? %))

;; clojure.test puts tests in metadata.
;; See line 4.
(defn join
  {:test (fn []
           (assert (= (join "," [1 2 3]) "1,2,3")))}
  [sep s]
  (apply str (interpose sep s))) 

(defn ^:private ^:dynamic sum [nums]
  (map + nums))
;; is the same as:
(defn ^{:private true, :dynamic true} sum [nums]
  (map + nums))
;; and
(defn sum {:private true, :dynamic true} [nums]
  (map + nums))
;; and
(defn sum
  ([nums]
   (map + nums))
  {:private true, :dynamic true})
;; The multitude of forms is useful for macros. Usually use shorthand.

;; HOFs

;; Sort can take just data, or a function & a seq
;; but fails when the contents of the seq aren't mutually comparable:
; (sort ["z" "x" "a" "aa" 1 5 8])
;; or something where you're sorting by contents:
; (sort [[:a 7], [:c 13], [:b 21]])
;; sort-by takes a fn to preprocess the elements into mutually comparable types:
(sort-by second [[:a 7], [:c 13], [:b 21]])
(sort-by second > [[:a 7], [:c 13], [:b 21]])

(sort-by str ["z" "x" "a" "aa" 1 5 8])
(sort-by :age [{:age 7} {:age 99} {:age 30}])

(def plays [{:band "Burial",     :plays 979,  :loved 9}
            {:band "Eno",        :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979,  :loved 9}
            {:band "Magma",      :plays 2665, :loved 31}])

(defn columns [column-names]
  (fn [row]
    ;; using #(% row) instead of just row b/c while maps can be used as a fn, records can't.
    (vec (map #(% row) column-names))))

;; A fallback sort
; (sort-by (columns [:plays :loved :band]) plays)
((columns [:plays :loved :band])
 {:band "Burial", :plays 979, :loved 9})

;; If a function of some arguments always results in the
;; same value and doesn't change anything else,
;; the reference to the function is transparent in time:
;; referentially transparent.
(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only)
            (map f (vals only)))))

; (keys-apply #(.toUpperCase %) #{:band} (plays 0))

(defn manip-map
  "Manipulate a set of keys based on a given function."
  [f ks m]
  (merge m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))

;; Referentially transparent functions are easier to optimize.

;; Named arguments!
(defn slope
  {:test (fn []
           (assert (= (slope :p1 [4 15] :p2 [3 21]) -6.0))  
           (assert (= (slope :p2 [2 1]) 0.5))  
           (assert (= (slope) 1.0)))}
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

;; Pre- and Post-conditions!!!!!!
;; Use pre & post instead of assert post of the time. 
;; Assert can be used when pre & post don't work, e.g. loop invariants.

;; To disable pre/post checks, run this near the top of the file:
; (set! *assert* false)
(defn slope-v2
  {:test (fn []
           (assert (= (slope-v2 [4 15] [3.0 21.0]) -6.0))  
           (assert (= :err (try (slope-v2 [1 1] [1 1]) (catch AssertionError e :err))))
           (assert (= :err (try (slope-v2 [1 1] '(1 2)) (catch AssertionError e :err)))))}  
  [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

;; Decoupling assertions
(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(defn vegan-constraints [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

; (vegan-constraints put-things {:veggie "carrot"})

(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(balanced-diet put-things {:veggie "carrot"})

;; By pulling assertions into a wrapper, you're decoupling domain-specific requirements
;; and isolating them into aspects.
(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]} ;; <-- never change meat
  (f m))

;; Closures
(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

; (add-and-get 2)

(defn times-n [x]
  (fn [y] (* y x)))

; ((times-n 4) 3)

;; Closures as a tidy bundle of values and related functions, kinda like an object.

;; Using cartesian coordinates.
(def bearings [{:x  0, :y  1} ; north
               {:x  1, :y  0} ; east
               {:x  0, :y -1} ; south
               {:x -1, :y  0}])  ; west

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

; (forward 5 5 0)

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-left (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-right (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:coords (bot 5 5 0))
(:bearing (bot 5 5 0))
(:bearing (bot 5 5 1))
(:bearing ((:turn-right (bot 5 5 1))))
(:bearing ((:turn-left (bot 5 5 2))))
(:coords ((:forward (bot 5 5 0))))

;; Polymorphismish
(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})

(:coords ((:forward (mirror-bot 5 5 0))))

;; Closures are compiled AOT and are very performant. :)

;; 2 major techniques for mundane (named explicitly, not through
;; mutual recursion or implicitly with recur) to tail recursion:
;; 1. Use a helper function to do most of the work.
;; 2. Use an accumulator, eliminating stack explosion.
;; For functions that generate sequences, lazy-seq is often better than
;; tail recursion b/c the mundane dfn is more natural & understandable.
(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

; (pow 2N 99999)


;; 7.3.4 CPS
;; It's a way of generalizing computation by viewing it in terms of up to 3 functions:
;;   Accept - Decides when a computation should terminate
;;   Return - Wraps the return values
;;   Continuation - Provides the next step in the computation
;; So the same rules as recursion, but may not be to itself.

;; Not widely used in clojure: unless the app can guarantee a bounded execution path, it will blow the stack.
;; It can make errors hard to track down.
;; In lazy langs like haskell, strict order isn't as important. But it's still not good with parallelization.

(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(defn mk-cps
  "A CPS function generator."
  [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v]
                    (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def fac2
  (mk-cps zero?
          identity
          #(* %1 %2)))


(t/run-tests)
