(ns unit-calc.core-test
  (:require [clojure.test :refer :all]
            [unit-calc.core :refer :all]))

(deftest metric-converter
  (let [mc (partial convert simple-metric)]
    (testing "metric units"
      (is (= 1 (mc [1 :meter])))
      (is (= 1000 (mc [1 :km])))   
      (is (= 1/100 (mc [1 :cm])))   
      (is (== 3010.81 (convert simple-metric [3 :km 10 :meter 80 :cm 10 :mm]))))))

(deftest data
  (let [dc (partial convert simple-data)]
    (testing "data units"
      (is (= 128N (dc [32 :nibble]))))))
