(ns aoc2024.day04.solution
  (:require
   [aoc2024.core :refer [read-single-line]]
   [clojure.string :as str]))

(def sample-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def char-map (->> (str/split-lines sample-input)
                   (map (fn [x] (str/split x #"")))))
char-map

(defn check-coord [x y m deltas]
  (let [coords (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)
        chars (map #(get-in m % nil) coords)]
    (= chars ["M" "A" "S"])))

(defn find-xmas [x y m]
  (let [directions {:up            [[0 -1] [0 -2] [0 -3]]
                    :down          [[0 1] [0 2] [0 3]]
                    :left          [[-1 0] [-2 0] [-3 0]]
                    :right         [[1 0] [2 0] [3 0]]
                    :top-right     [[1 -1] [2 -2] [3 -3]]
                    :bottom-right  [[1 1] [2 2] [3 3]]
                    :bottom-left   [[-1 1] [-2 2] [-3 3]]
                    :top-left      [[-1 -1] [-2 -2] [-3 -3]]}]
    (->> directions
         vals
         (filter #(check-coord x y m %))
         count)))

(defn find-coord-for-char [char char-map]
  (for [x (range (count (first char-map)))
        y (range (count char-map))
        :when (= char (get-in char-map [x y]))]
    [x y]))


(defn solve1 [input]
  (let [char-map (mapv #(vec (str/split % #"")) input)]
    (->> (find-coord-for-char "X" char-map)
         (map (fn [[x y]] (find-xmas x y char-map)))
         (reduce +))))

(solve1 (str/split-lines sample-input))
(solve1 (str/split-lines (read-single-line "resources/day04/input.txt")))

;; part 2

(defn check-mmss [x y m]
  (and
   (= "M" (get-in m [(- x 1) (- y 1)])
      (get-in m [(- x 1) (+ y 1)]))
   (= "S" (get-in m [(+ x 1) (+ y 1)])
      (get-in m [(+ x 1) (- y 1)]))))

(defn check-smms [x y m]
  (and
   (= "M" (get-in m [(- x 1) (+ y 1)])
      (get-in m [(+ x 1) (+ y 1)]))
   (= "S" (get-in m [(+ x 1) (- y 1)])
      (get-in m [(- x 1) (- y 1)]))))

(defn check-ssmm [x y m]
  (and
   (= "S" (get-in m [(- x 1) (- y 1)])
      (get-in m [(- x 1) (+ y 1)]))
   (= "M" (get-in m [(+ x 1) (+ y 1)])
      (get-in m [(+ x 1) (- y 1)]))))

(defn check-mssm [x y m]
  (and
   (= "S" (get-in m [(- x 1) (+ y 1)])
      (get-in m [(+ x 1) (+ y 1)]))
   (= "M" (get-in m [(+ x 1) (- y 1)])
      (get-in m [(- x 1) (- y 1)]))))

(defn find-x-mas [x y m]
  (->> [check-mmss
        check-smms
        check-ssmm
        check-mssm]
       (filter (fn [f]
                 (f x y m)))
       (count)))

(defn solve2 [input]
  (let [char-map (mapv #(vec (str/split % #"")) input)]
    (->> (find-coord-for-char "A" char-map)
         (map (fn [[x y]] (find-x-mas x y (vec char-map))))
         (reduce +))))

(solve2 (str/split-lines sample-input))
(solve2 (str/split-lines (read-single-line "resources/day04/input.txt")))
