(ns chapter-7.core
  (:require [clojure.test :as t]))

(t/run-tests)

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



