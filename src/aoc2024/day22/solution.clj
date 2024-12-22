(ns aoc2024.day22.solution
  (:require [clojure.string :as str]))

(str/blank? " ")

; bitwise xor
(bit-xor 42 15)

(defn mix [secret-number num-to-mix]
  (bit-xor secret-number num-to-mix))

(mix 123 7872)
(mix 42 15)

(defn prune [num-to-prune]
  (mod num-to-prune 16777216))

(prune 100000000)

(prune 7867)

(defn op-1 [num]
  (->> num
       (* 64)
       (mix num)
       (prune)))

(op-1 123)

(defn op-2 [num]
  (->> num
       (#(/ % 32))
       (Math/floor)
       (int)
       (mix num)
       (prune)))

(op-2 7867)

(defn op-3 [num]
  (->> num
       (* 2048)
       (mix num)
       (prune)))

(op-3 7758)


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

(parse-input sample-input)

(loop [n 0
       secret-number 123]
  (if (< n 3)
    (recur (inc n) (ops secret-number))
    secret-number))

(defn calculate-secret-number [times initial-secret-number]
  (loop [n 0
         secret-number initial-secret-number]
    (if (< n times)
      (recur (inc n) (ops secret-number))
      secret-number)))

(calculate-secret-number 2000 123)

(defn solve1 [input]
  (->> input
       (parse-input)
       (map #(calculate-secret-number 2000 %))
       (reduce +)))

(solve1 sample-input)
(solve1 (slurp "resources/day22/input.txt"))
