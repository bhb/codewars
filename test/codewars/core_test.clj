(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer :all]))

;; (deftest powers-test
;;   (is (= [[]] (powers [])))
;;   (is (= [[] [1]] (powers [1])))
;;   (is (= [[] [1] [2] [1 2]] (powers [1 2])))
;;   (is (= [[],
;;           [1],
;;           [2],
;;           [3],
;;           [1,2],
;;           [1,3],
;;           [2,3],
;;           [1,2,3]] (powers [1 2 3]))))

(deftest powers-test
  (is (= 1 (powers [])))
  (is (= 2 (powers [1])))
  (is (= 4 (powers [1 2])))
  (is (= 8 (powers [1 2 3]))))

(deftest example-nth-term-test
  (is (= (nth-term 1 2 3) 7) )
  (is (= (nth-term 2 2 2) 6) )
  (is (= (nth-term -50 10 20) 150) )
)

(run-tests)


