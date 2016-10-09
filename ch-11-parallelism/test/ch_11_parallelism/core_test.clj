(ns ch-11-parallelism.core-test
  (:use [ch-11-parallelism.dothreads :refer [dothreads!]])
  (:require [clojure.test :exclude (run-tests)] ;:refer :all]
            [ch-11-parallelism.core :refer :all]))

(defrecord TestRun [run passed failed])

(defn pass [] true)
(defn fail [] false)

(defn run-tests [& all-tests]
  (with-promises
    [tests all-tests :as results]
    (into (->TestRun 0 0 0)
          (reduce #(merge-with + %1 %2) {}
                  (for [r results]
                    (if @r
                      {:run 1 :passed 1}
                      {:run 1 :failed 1})))))) 

(run-tests pass fail fail fail pass)

