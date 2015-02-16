(ns codewars.core)

(defmacro -dbg
  [x]
  `~x)

(defmacro dbg
  [x]
  `(let [x# ~x]
     (clojure.pprint/pprint (str "dbg:" '~x "="))
     (clojure.pprint/pprint x#)
     x#))

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

(defn find-the-ball1
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



(defn find-the-ball
  "Given the starting position and a list of swaps, find the final position"
  [initial-position moves]
  (reduce (fn [pos [from to]]
            (condp = pos
              from to
              to from
              pos)) initial-position moves))

(find-the-ball 0 [[0 1]])

(defn multiple-sum [n]
  (letfn [(divisible-by? [x y] (zero? (mod x y)))]
    (->> n
         (range)
         (filter #(or (divisible-by? % 3) (divisible-by? % 5)))
         (apply +))))

(defn desc-order [n]
  (-> n
      str
      (clojure.string/split #"")
      sort
      reverse
      clojure.string/join
      Integer/parseInt))

(defn mean [xs]
  (/ (apply + xs) (count xs)))

(defn estimated-syllables [s]
 (count (re-seq #"(?i)[aeiouy]+" s)))

(defn round [x]
  (* 0.01 (Math/round (* 100 x))))

(defn find-words [s]
  (remove empty? (clojure.string/split s #" +")))

(defn flesch-kincaid [text]
  (let [sentences (clojure.string/split text #"[!\.\?]")
        words-per-sentence (map (comp count find-words) sentences)
        syllables (map estimated-syllables (find-words text))]
    (round
     (- (+ (* 11.8
              (mean syllables))
           (* 0.39
              (mean words-per-sentence)))
        15.59))))

