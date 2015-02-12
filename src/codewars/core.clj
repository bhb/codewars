(ns codewars.core)

(defn powers1 [num count]
  (if (zero? count)
    num
    (recur (*' num 2) (dec count))))

(defn powers [list]
  (powers1 1 (count list)
  ))

(powers [1 2 3])

(powers [])
(powers [1])
(powers [1 2])

(powers (range 0 10))

;; #_(= (count (powers x))
;;      (powers-count x))



;; (defn nth-term [first n c]
;;   (nth (iterate (partial + c) first) n))

(defn nth-term [first n c]
  (as-> first $
       (iterate (partial + c) $)
       (nth $ n)))

(take 100 (range))

(nth-term 1 2 3)
(nth-term 2 2 2)
(nth-term -50 10 20)
