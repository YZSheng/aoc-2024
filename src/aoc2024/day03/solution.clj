(ns aoc2024.day03.solution
  (:require
   [aoc2024.core :refer [read-single-line]]
   [clojure.string :as str]))

(def sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def sample-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn solve1 [input]
  (->> input
       (re-seq #"mul\(\d+,\d+\)")
       (map (fn [match]
              (let [nums (map #(Integer/parseInt %) (re-seq #"\d+" match))]
                (apply * nums))))
       (reduce +)))

(solve1 (read-single-line "resources/day03/input.txt"))

(defn solve2 [input]
  (->> (str/split input #"do\(\)")
       (map (fn [s] (first (str/split s #"don't\(\)"))))
       (map solve1)
       (reduce +)))

(solve2 (read-single-line "resources/day03/input.txt"))