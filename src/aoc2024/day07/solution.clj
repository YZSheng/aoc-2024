(ns aoc2024.day07.solution
  (:require [clojure.string :as str]))

(def sample-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"\d+" %))
       (map (fn [xs] (map #(Long/parseLong %) xs)))))

(defn evaluation-candidates [first-number rest-numbers f]
  (concat [first-number]
          (vector (f (first rest-numbers) (second rest-numbers)))
          (rest (rest rest-numbers))))

(defn evaluate-result [numbers fns]
  (let [first-number (first numbers)
        rest-numbers (rest numbers)]
    (if (= 2 (count numbers))
      (= first-number (second numbers))
      (let [partial-evaluation-candidates (partial evaluation-candidates first-number rest-numbers)]
        (some #(evaluate-result % fns) (map #(partial-evaluation-candidates %) fns))))))

(defn solve [input fns]
  (->> (parse-input input)
       (filter #(evaluate-result % fns))
       (map first)
       (reduce +)))

(defn solve1 [input]
  (solve input [* +]))

(solve1 sample-input)
(solve1 (slurp "resources/day07/input.txt"))

(defn merge-numbers [a b]
  (Long/parseLong (str a b)))

;; part 2
(defn solve2 [input]
  (solve input [* + merge-numbers]))

(solve2 sample-input)
(solve2 (slurp "resources/day07/input.txt"))