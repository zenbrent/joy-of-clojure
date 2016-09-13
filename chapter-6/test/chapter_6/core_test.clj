(ns chapter-6.core-test
  (:require [clojure.test :refer :all]
            [chapter-6.core :refer :all]))

(deftest xseq-test
  (testing "xseq"
    (is (= (xseq {:val 5 :L {:val 4 :L nil :R nil} :R {:val 2 :L nil :R nil}})
           '(4 5 2)))))

(deftest xconj-test
  (testing "xconj"
    (is (= (xseq (xconj nil 5))
           '(5)))  
    (is (= (xseq (xconj (xconj nil 5) 6))
           '(5 6)))  
    (is (= (xseq (xconj (xconj nil 6) 5))
           '(5 6)))  
    (is (= (xseq (xconj (xconj (xconj nil 5) 7) 6))
           '(5 6 7)))))

(deftest shared-structure
  (testing "the structure of the trees is shared"
    (let [t1 (xconj nil 5)
          t2 (xconj t1 6)
          t3 (xconj t1 4)]
      (is (identical? (:L t1)
                      (:L t2)))   
      (is (identical? (:L t1)
                      (:R t3))))))
