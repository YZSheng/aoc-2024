(ns aoc2024.day02.solution
  (:require
   [aoc2024.core :refer [read-lines]]
   [clojure.string :as str]))

(def sample-input (str/split-lines
                   "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))

(defn is-safe [pairs]
  (or (every? (fn [[a b]] (and (< a b) (>= 3 (- b a)))) pairs)
      (every? (fn [[a b]] (and (> a b) (>= 3 (- a b)))) pairs)))

(defn solve1 [input]
  (->> input
       (map #(str/split % #"\s+"))
       (map (fn [x] (map #(Integer/parseInt %) x)))
       (map (fn [x] (partition 2 1 x)))
       (filter is-safe)
       count))

(solve1 sample-input)
(solve1 (read-lines "resources/day02/input.txt"))

(defn sublists-with-one-removed [items]
  (map-indexed
   (fn [idx _]
     (concat (take idx items) (drop (inc idx) items)))
   items))

(defn is-safe-if-remove-one [nums]
  (if (is-safe (partition 2 1 nums)) true
      (some #(is-safe (partition 2 1 %)) (sublists-with-one-removed nums))))

(defn solve2 [input]
  (->> input
       (map #(str/split % #"\s+"))
       (map (fn [x] (map #(Integer/parseInt %) x)))
       (filter is-safe-if-remove-one)
       count))

(solve2 sample-input)
(solve2 (read-lines "resources/day02/input.txt"))