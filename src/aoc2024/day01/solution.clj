(ns aoc2024.day01.solution
  (:require
   [aoc2024.core :refer [read-lines transpose]]
   [clojure.string :as str]))

(def sample-input "3   4
4   3
2   5
1   3
3   9
3   3")

(def input (str/split-lines sample-input))

(defn solve [input]
  (->> input
       (map #(str/split % #"\s+"))
       (map (fn [x] (map #(Integer/parseInt %) x)))
       transpose
       (map sort)
       transpose
       (map (fn [[a b]] (Math/abs (- a b))))
       (reduce +)))

(solve input)
(solve (read-lines "resources/day01/input.txt"))

(defn solve2 [input]
  (let [[a b] (->> input
                   (map #(str/split % #"\s+"))
                   (map (fn [x] (map #(Integer/parseInt %) x)))
                   transpose)
        freq (frequencies b)]
    (reduce + (map (fn [x] (* x (get freq x 0))) a))))

(solve2 input)
(solve2 (read-lines "resources/day01/input.txt"))
