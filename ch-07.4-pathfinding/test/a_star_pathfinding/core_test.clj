(ns a-star-pathfinding.core-test
  (:require [clojure.test :refer :all]
            [a-star-pathfinding.core :refer :all]))

(deftest neighbors-test
  (testing "finding neighbors in a path at a given point"
    (is (= (neighbors 5 [0 0]) [[1 0] [0 1]]))
    (is (= (neighbors 5 [2 0]) [[1 0] [3 0] [2 1]]))
    (is (= (neighbors 5 [2 1]) [[1 1] [3 1] [2 0] [2 2]]))))

(deftest estimate-cost-test
  (testing "Estimating the cost based on the cost of each step."
    (is (= (estimate-cost 900 5 0 0) 7200))
    (is (= (estimate-cost 900 5 4 4) 0))))

(deftest path-cost-test
  (testing "estimating the cost of the path so far"
    (is (= (path-cost 900 {:cost 1}) 901))))

(deftest total-cost-test
  (testing "the total cost of a path"
    (is (= (total-cost 0 900 5 0 0) 7200))
    (is (= (total-cost 1000 900 5 3 4) 1900))
    (is (= (total-cost (path-cost 900 {:cost 1}) 900 5 3 4) 1801))))

(deftest min-by-test
  (testing "finding the min by a function"
    (is (= {:cost 1} (min-by :cost [{:cost 100} {:cost 110} {:cost 1} {:cost 99} {:cost 1000}])))
    (is (= {:cost -10} (min-by :cost [{:cost 10} {:cost -10}])))))

(deftest astar-test
  (testing "find the best path"
    (is (=
         (astar [0 0] 17 world)
         [{:cost 17,
           :yxs [[0 0]
                 [0 1]
                 [0 2]
                 [0 3]
                 [0 4]
                 [1 4]
                 [2 4]
                 [2 3]
                 [2 2]
                 [2 1]
                 [2 0]
                 [3 0]
                 [4 0]
                 [4 1]
                 [4 2]
                 [4 3]
                 [4 4]]}
          :steps 81]))
    (is (=
         (astar [0 0] 900 world2)
         [{:cost 9
           :yxs [[0 0] [0 1] [0 2] [1 2] [2 2] [3 2] [4 2] [4 3] [4 4]]}
          :steps 134]))
    (is (=
         (astar [0 0] 900 (assoc-in world2 [4 3] 666))
         [{:cost 10
           :yxs [[0 0] [0 1] [0 2] [0 3] [0 4] [1 4] [2 4] [3 4] [4 4]]}
          :steps 132]))))
