(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer :all :as unpack] :reload))

;; ==========================================================================

(deftest powers-test
  (is (= 1 (powers [])))
  (is (= 2 (powers [1])))
  (is (= 4 (powers [1 2])))
  (is (= 8 (powers [1 2 3]))))

;; ==========================================================================

(deftest example-nth-term-test
  (is (= (nth-term 1 2 3) 7) )
  (is (= (nth-term 2 2 2) 6) )
  (is (= (nth-term -50 10 20) 150) )
)

;; ==========================================================================

(deftest examples
  (is (= (unpack/spread + [1 2 3 4 5]) 15), "Spread isn't working!")
)

;; ==========================================================================

(deftest basic-test
 (testing "An empty swap set doesn't move the ball"
   (is (= (find-the-ball 5 []) 5)))
 (testing "Some games"
   (is (= (find-the-ball 0 [[0 1]])) 1)
   (is (= (find-the-ball 0 [[1 0]])) 1)
   (is (= (find-the-ball 0 [[0 1] [2 1] [0 1]]) 2))))

;; ==========================================================================

(deftest multiple-sum-test
  (is (= (multiple-sum 0) 0))
  (is (= (multiple-sum 4) 3))
  (is (= (multiple-sum 6) 8))
  (is (= (multiple-sum 10) 23)))

;; ==========================================================================

(deftest test-cases
  (are [n expected]
    (= expected (desc-order n))
    0          0
    1          1
    15         51
    812        821))

;; ==========================================================================

(defn =? [a b] (= (format "%.2f" a) (format "%.2f" b)))

(def cases [
   [ "The turtle is leaving." 3.67]
   [ "A good book is hard to find." -1.06]
   [ "To be or not to be. That is the question." -0.66]
   [ "Oh no! The lemming is falling." 1.31]
   [ "Do not cut your fingers as your katana is getting sharper! Be gentle." 4.19]])

(deftest deterministic-tests
  (doseq [[text val] cases]
         (testing text (is (=? (flesch-kincaid text) val)))))

;; ==========================================================================

(deftest Testing...
  (is (= (zeros10 0) 0) "Zero has 0 trailing zeros")
  (is (= (zeros10 6) 1))
  (is (= (zeros10 30) 7))
)

(run-tests)

