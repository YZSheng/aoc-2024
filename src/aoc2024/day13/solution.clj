(ns aoc2024.day13.solution
  (:require [clojure.string :as str]))

(def sample-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn parse-line [line]
  (->> line
       (apply str)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       ((fn [[a-delta-x
              a-delta-y
              b-delta-x
              b-delta-y
              target-x
              target-y]] {:a [a-delta-x a-delta-y]
                          :b [b-delta-x b-delta-y]
                          :target [target-x target-y]}))))

(parse-line ["Button A: X+94, Y+34" "Button B: X+22, Y+67" "Prize: X=8400, Y=5400"])

(defn find-a-b-combinations [a b target max]
  (let [[a-delta-x a-delta-y] a
        [b-delta-x b-delta-y] b
        [target-x target-y] target]
    (->> (for [a-count (range 0 max)
               b-count (range 0 max)
               :let [x-result (+ (* a-count a-delta-x) 
                                (* b-count b-delta-x))
                     y-result (+ (* a-count a-delta-y)
                                (* b-count b-delta-y))]
               :when (and (= x-result target-x)
                         (= y-result target-y))]
           [a-count b-count]))))

(find-a-b-combinations [94 34] [22 67] [8400 5400] 100)
(find-a-b-combinations [26 66] [67 21] [12748 12176] 100)
(find-a-b-combinations [17 86] [84 37] [7870 6450] 100)
(find-a-b-combinations [69 23] [27 71] [18641 10279] 100)

(defn calculate-token-count [a-count b-count]
  (+ (* a-count 3) b-count))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (filter #(not (str/blank? %)))
       (partition 3)
       (map parse-line)))

(parse-input sample-input)

(defn solve1 [input]
  (->> input
       parse-input
       (map parse-line)
       (map (fn [{:keys [a b target]}]
              (find-a-b-combinations a b target 100)))
       (filter not-empty)
       (map first)
       (map (fn [[a-count b-count]]
              (calculate-token-count a-count b-count)))
       (reduce +)))

(solve1 sample-input)
(solve1 (slurp "resources/day13/input.txt"))