(ns chapter-8.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [chapter-8.core :refer :all]))

(deftest contextual-eval-test
  (testing "Eval with less evil."
    (is (= 3 (contextual-eval '{a 1 b 2} '(+ a b))))
    (is (= 1001 (contextual-eval '{a 1 b 2} '(let [b 1000] (+ a b)))))))

(deftest def-watched-test
  (testing "Watching values with callbacks"
    (is (= (string/split-lines
             (with-out-str (def-watched x 2)
                           (def x 3)  
                           (def x 0)))
           ["2  ->  3" "3  ->  0"]))))

