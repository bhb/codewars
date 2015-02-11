(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer :all]))

(deftest powers-test
  (is (= [[]] (powers [])))
  (is (= [[] [1]] (powers [1])))
  (is (= [[] [1] [2] [1 2]] (powers [1 2])))
  (is (= [[],
          [1],
          [2],
          [3],
          [1,2],
          [1,3],
          [2,3],
          [1,2,3]] (powers [1 2 3]))))

(run-tests)
