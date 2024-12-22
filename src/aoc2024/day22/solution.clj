(ns aoc2024.day22.solution
  (:require [clojure.string :as str]))

(defn mix [secret-number num-to-mix]
  (bit-xor secret-number num-to-mix))

(defn prune [num-to-prune]
  (mod num-to-prune 16777216))

(defn op-1 [num]
  (->> num
       (* 64)
       (mix num)
       (prune)))

(defn op-2 [num]
  (->> num
       (#(/ % 32))
       (Math/floor)
       (int)
       (mix num)
       (prune)))

(defn op-3 [num]
  (->> num
       (* 2048)
       (mix num)
       (prune)))

(defn ops [num]
  (->> num
       (op-1)
       (op-2)
       (op-3)))

(def sample-input "1
10
100
2024")

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-long)))

(defn calculate-secret-number [times initial-secret-number]
  (loop [n 0
         secret-number initial-secret-number]
    (if (< n times)
      (recur (inc n) (ops secret-number))
      secret-number)))

(defn solve1 [input]
  (->> input
       (parse-input)
       (map #(calculate-secret-number 2000 %))
       (reduce +)))

(solve1 sample-input)
(solve1 (slurp "resources/day22/input.txt"))

;; part 2

(defn to-one-digit [num]
  (Character/digit (last (str num)) 10))

(defn generate-one-digit-prices [times initial-price]
  (loop [n 0
         secret-number initial-price
         result [(to-one-digit initial-price)]]
    (if (< n (dec times))
      (recur (inc n) (ops secret-number) (conj result (to-one-digit (ops secret-number))))
      result)))

(defn into-price-change-map [xs]
  (reduce (fn [acc [k v]]
            (if (contains? acc k)
              acc
              (assoc acc k v)))
          {}
          xs))

(defn group-sell-price-by-price-change [secret-number-times original-secret-number]
  (->> (generate-one-digit-prices secret-number-times original-secret-number)
       (partition 2 1)
       (map (fn [[from to]] [from to (- to from)]))
       (partition 4 1)
       (group-by (fn [xs] (mapv last xs)))
       (map (fn [[k v]] [k (->> v first last second)]))
       (into-price-change-map)))

(defn solve2 [input n]
  (let [sell-price-maps (->> input
                             (parse-input)
                             (map #(group-sell-price-by-price-change n %)))
        price-sums (->> sell-price-maps
                        (apply merge-with +))]
    (->> price-sums
         (vals)
         (apply max))))

(def part-2-sample-input "1
2
3
2024")

(solve2 part-2-sample-input 2000)
(solve2 (slurp "resources/day22/input.txt") 2000)

(comment

  (bit-xor 42 15)

  (mix 123 7872)
  (mix 42 15)

  (prune 100000000)

  (prune 7867)
  (calculate-secret-number 2000 123)
  (generate-one-digit-prices 10 123)
  (->> sample-input
       (parse-input)
       (map #(group-sell-price-by-price-change 10 %))
       (mapcat (fn [m] (keys m)))
       distinct)

  (->> (generate-one-digit-prices 10 123)
       (partition 2 1)
       (map (fn [[from to]] [from to (- to from)]))
       (partition 4 1)
       (group-by (fn [xs] (mapv last xs)))
       (map (fn [[k v]] [k (->> v first last second)])))

  (into-price-change-map '([[-3 6 -1 -1] 4] [[6 -1 -1 0] 4] [[-1 -1 0 2] 6] [[-1 0 2 -2] 4] [[0 2 -2 0] 4] [[2 -2 0 -2] 2])))