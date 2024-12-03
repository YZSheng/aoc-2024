(ns aoc2024.day03.solution
  (:require
   [aoc2024.core :refer [read-single-line]]))

(def sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

sample-input

(re-seq #"mul\(\d+,\d\)" sample-input)

(->> sample-input
     (re-seq #"mul\(\d+,\d\)")
     (map (fn [match]
            (let [nums (map #(Integer/parseInt %) (re-seq #"\d+" match))]
              (apply * nums))))
     (reduce +))

(->> (read-single-line "resources/day03/input.txt")
     (re-seq #"mul\(\d+,\d+\)")
     (map (fn [match]
            (let [nums (map #(Integer/parseInt %) (re-seq #"\d+" match))]
              (apply * nums))))
     (reduce +))
