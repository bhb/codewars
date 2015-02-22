(ns codewars.core
  (:require
   [criterium.core :refer [quick-bench with-progress-reporting]]
   [taoensso.timbre.profiling :as profiler :refer [profile sampling-profile p]]
    ))

(defmacro -dbg
  [x]
  `~x)

(defmacro dbg
  [x]
  `(let [x# ~x]
     (clojure.pprint/pprint (str "dbg:" '~x "="))
     (clojure.pprint/pprint x#)
     x#))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

;; ==========================================================================

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


;; ==========================================================================

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

;; ==========================================================================

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

;; ==========================================================================

(defn multiple-sum [n]
  (letfn [(divisible-by? [x y] (zero? (mod x y)))]
    (->> n
         (range)
         (filter #(or (divisible-by? % 3) (divisible-by? % 5)))
         (apply +))))

;; ==========================================================================

(defn desc-order [n]
  (-> n
      str
      (clojure.string/split #"")
      sort
      reverse
      clojure.string/join
      Integer/parseInt))

;; ==========================================================================

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

;; ==========================================================================


(defn fact
  ([x] (fact (bigint x) 1))
  ([x product]
     (condp = x
       0 0
       1 product
       (recur (dec x) (* x product)))))

(defn fact2 [n] (reduce *' (range 1 (inc n))))

(defn zeros [n]
  (if (zero? n)
    0
    (->> n
         fact2
         str
         (re-find #"0+$")
         count
         )))

(defn primes-under* [x]
  (loop [candidates (range 2 x)
         last-prime 1]
    (let [prime (first (filter #(< last-prime %) candidates))]
      (if (nil? prime)
        candidates
        (recur (remove #(and (zero? (mod % prime))
                             (not= prime %)) candidates)
               prime)))))

(def primes-under (memoize primes-under*))

(defn prime? [x]
  (or (= x 1)
      (in? (primes-under (inc x)) x)))

(primes-under 140)

(defn factors* [x]
  {:post [(= x (apply * %))]}
  (if (prime? x)
    (list x)
    (let [first-factor (first (filter #(zero? (mod x %)) (primes-under x)))
          factors- [first-factor (/ x first-factor)]]
      (if (every? prime? factors-)
        factors-
        (flatten (map factors* factors-))))))

(def factors (memoize factors*))

(defn zeros2 [n]
  (let [factors- (mapcat factors (range 2 (inc n)))
        twos (filter #(= 2 %) factors-)
        fives (filter #(= 5 %) factors-)]
    (min (count twos)
         (count fives))
    ))

(defn divisible? [x y]
  (zero? (mod x y)))

(defn zeros3
  ([x] (zeros3 x 1 0))
  ([x product zeros]
     (condp = x
       0 0
       1 zeros
       (let [new-product (*' x product)
             quotient (/ new-product 10)]
         (if (integer? quotient)
           (recur (dec x) quotient (inc zeros))
           (recur (dec x) new-product zeros))))))

(defn subfactorial [x]
  (reduce *' (filter #(or (divisible? % 2)
                          (divisible? % 5)) (range 1 (inc x)))))

(defn zeros4 [n]
  (if (zero? n)
    0
    (->> n
         subfactorial
         str
         (re-find #"0+$")
         count
         )))

(defn zeros5 [x]
  (loop [i 1
         product 1
         zeros 0]
    #_(dbg [i product zeros])
    (cond
     (> i x) (->> product
                  ;;str
                  ;;(re-find #"0+$")
                  ;;count
                  ;;(+ zeros)
                  )
     ;;(divisible? i 10) (recur (inc i) (* (/ i 10) product) (inc zeros))
     (or (divisible? i 2)
         (divisible? i 5)
         (divisible? i 10)) (recur (inc i) (*' product i) zeros)
     :else (recur (inc i) product zeros))))

(defn twos-fives-tens-in-factors
  "Finds all factors of a number that are 2, 5, or 10.
   Returns a map e.g. {:twos 2 :fives 0 :tens 1} for x = 40"
  ([x] (twos-fives-tens-in-factors x {:twos 0 :fives 0 :tens 0}))
  ([x {:keys [twos fives tens] :as acc}]
  (let [quotient-ten (/ x 10)]
    (if (integer? quotient-ten)
      (recur quotient-ten { :twos twos :fives fives :tens (inc tens)})
      (let [quotient-two (/ x 2)]
        (if (integer? quotient-two)
          (recur quotient-two { :twos (inc twos) :fives fives :tens tens})
          (let [quotient-five (/ x 5)]
            (if (integer? quotient-five)
              (recur quotient-five { :twos twos :fives (inc fives) :tens tens})
              acc))))))))

(defn zeros6 [x]
  (loop [i 1
         acc { :twos 0 :fives 0 :tens 0 }]
    (if (> i x)
      (let [{:keys [twos fives tens]} acc]
        (+ tens (min twos fives)))
      (recur (inc i)
             (merge-with +
                         acc
                         (twos-fives-tens-in-factors i))))))

(defn zeros7 [x]
  (loop [i 1
         all-twos 0
         all-fives 0
         all-tens 0]
    (if (> i x)
      (+ all-tens (min all-twos all-fives))
      (let [{:keys [twos fives tens]} (twos-fives-tens-in-factors i)]
        (recur (inc i)
               (+ twos all-twos)
               (+ fives all-fives)
               (+ tens all-tens))))))

(defn times-divisble [y x]
  "Number of times x is divisible by y"
  (loop [i 0
         q x]
    (if (zero? (mod q y))
      (recur (inc i) (/ q y))
      i)))

(defn zeros8 [x]
  #_(->> (range 1 (inc x))
       (map (partial times-divisble 5))
       (reduce +))
  (reduce +
          (map (partial times-divisble 5)
               (range 1 (inc x))))
  )

(defn zeros9 [x]
  (loop [i 1
         zero-count 0]
    (if (> i x)
      zero-count
      (let [times (times-divisble 5 i)]
        (recur (inc i) (+ times zero-count))))))

(defn zeros10 [x]
  (loop [i 5
         count 0]
    (if (> i x)
      (int count)
      (recur (* 5 i) (+ count (Math/floor (/ x i)))))))


  ;; (int (+ (Math/floor (/ x 5))
  ;;    (Math/floor (/ x 25))
  ;;    (Math/floor (/ x 125))
  ;;    (Math/floor (/ x (* 125 5)))
  ;;    (Math/floor (/ x (* 125 5 5)))
  ;;    (Math/floor (/ x (* 125 5 5 5)))
  ;;    (Math/floor (/ x (* 125 5 5 5 5)))
  ;;    (Math/floor (/ x (* 125 5 5 5 5 5)))
  ;;    )))

(zeros10 50)
(zeros 50)

;;(time (zeros 10000))
;;(time (zeros8 10000))

;;(zeros 100000)
;;(zeros2 50)
;;(zeros6 50)
;;
(let [x 1000000]
  [(zeros8 x) (zeros10 x)])

;;(time (zeros 10000))
;;(time (zeros6 10000))
;;(time (zeros7 10000))
;;(time (zeros7 1000000)) ;;249998
;;(time (zeros8 1000000)) ;;249998
;;(time (zeros9 1000000)) ;;249998
;;(time (zeros10 1000000))

;;(profile :info :test (zeros8 10000))

#_(every?
   #(= (zeros9 %)
       (zeros10 %))
   (range 0 10000))


;; idea - every time you multiply by next number, if current product
;; is divisible by ten, then divide by 10 and inc num of tens
;; you can still count at end


;; (def zeros2 [n]
;;   (let [nums (range 1 (inc n))
;;         tens (filter #(mod

