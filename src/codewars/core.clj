(ns codewars.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn- powers* [acc xs]
  (if (empty? xs)
    acc
    (let [[first & rest] xs
          conj-first (map #(conj % first) acc)]
      (recur (vec (concat acc conj-first)) rest))))

(defn powers [xs]
  (sort (powers* [[]] xs))
  )

(powers [])
(powers [1])
(powers [1 2])

(def x (vec (range 0 100)))

(count (powers x))
