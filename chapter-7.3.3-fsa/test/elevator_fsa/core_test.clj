(ns elevator-fsa.core-test
  (:require [clojure.test :refer :all]
            [elevator-fsa.core :refer :all]))

(deftest elevator-tests
  (testing "Some states"
    (is (= false (elevator [:close :open :close :open :close :down :up :down :open]))) 
    (is (= true (elevator (conj (vec (take 10 (cycle [:close :open]))) :done))))
    (is (= false (elevator (conj (vec (take 11 (cycle [:close :open]))) :done))))
    (is (= true (elevator [:close :up :open :close :down :open :done])))))

