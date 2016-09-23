(ns chapter-7.core-test
  (:require [clojure.test :refer :all]
            [chapter-7.core :refer :all]))

(deftest fac2-test
  (testing "Factorials"
    (is (= (fac2 0) 1))
    (is (= (fac2 1) 1))
    (is (= (fac2 50N) 30414093201713378043612608166064768844377641568960512000000000000N))
    (is (= (fac2 2) 2))))

(run-tests)

