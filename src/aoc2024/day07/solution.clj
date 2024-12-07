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

(parse-input sample-input)


(defn evaluate-result [numbers]
  (let [first-number (first numbers)
        rest-numbers (rest numbers)]
    (if (= 2 (count numbers))
      (= first-number (second numbers))
      (or (evaluate-result (concat [first-number]
                                   (vector (* (first rest-numbers) (second rest-numbers)))
                                   (rest (rest rest-numbers))))
          (evaluate-result (concat [first-number]
                                   (vector (+ (first rest-numbers) (second rest-numbers)))
                                   (rest (rest rest-numbers))))))))

(evaluate-result '(190 10 19))
(evaluate-result '(3267 81 40 27))

(->> (parse-input sample-input)
     (filter evaluate-result)
     (map first)
     (reduce +))

(->> (parse-input (slurp "resources/day07/input.txt"))
     (filter evaluate-result)
     (map first)
     (reduce +))
