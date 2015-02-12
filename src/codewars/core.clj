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

(defn spread [func args]
  (apply func args))

(spread + [1 2 3 4 5])

(defn find-the-ball
  "Given the starting position and a list of swaps, find the final position"
  [initial-position moves]
  (loop [position initial-position
         remaining-moves moves]
    (if (empty? remaining-moves)
      position
      (let [[from to] (first remaining-moves)
            new-position (cond
                          (= from position) to
                          (= to position) from
                          :else position)]
        (recur new-position (rest remaining-moves))
  ))))
