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


sample-input


(def char-map (->> (str/split-lines sample-input)
                   (map (fn [x] (str/split x #"")))))

char-map

(defn check-coord-down [x y m]
  (and
   (= "M" (get-in m [x (+ 1 y)]))
   (= "A" (get-in m [x (+ 2 y)]))
   (= "S" (get-in m [x (+ 3 y)]))))


(defn check-coord-up [x y m]
  (and
   (= "M" (get-in m [x (- y 1)]))
   (= "A" (get-in m [x (- y 2)]))
   (= "S" (get-in m [x (- y 3)]))))


(defn check-coord-right [x y m]
  (and
   (= "M" (get-in m [(+ x 1) y]))
   (= "A" (get-in m [(+ x 2) y]))
   (= "S" (get-in m [(+ x 3) y]))))


(defn check-coord-left [x y m]
  (and
   (= "M" (get-in m [(- x 1) y]))
   (= "A" (get-in m [(- x 2) y]))
   (= "S" (get-in m [(- x 3) y]))))


(defn check-coord-top-right [x y m]
  (and
   (= "M" (get-in m [(+ x 1) (- y 1)]))
   (= "A" (get-in m [(+ x 2) (- y 2)]))
   (= "S" (get-in m [(+ x 3) (- y 3)]))))


(defn check-coord-bottom-right [x y m]
  (and
   (= "M" (get-in m [(+ x 1) (+ y 1)]))
   (= "A" (get-in m [(+ x 2) (+ y 2)]))
   (= "S" (get-in m [(+ x 3) (+ y 3)]))))


(defn check-coord-bottom-left [x y m]
  (and
   (= "M" (get-in m [(- x 1) (+ y 1)]))
   (= "A" (get-in m [(- x 2) (+ y 2)]))
   (= "S" (get-in m [(- x 3) (+ y 3)]))))


(defn check-coord-top-left [x y m]
  (and
   (= "M" (get-in m [(- x 1) (- y 1)]))
   (= "A" (get-in m [(- x 2) (- y 2)]))
   (= "S" (get-in m [(- x 3) (- y 3)]))))


(get-in [[1 2] [3 4]] [-1 -1])

(count (first char-map))

(get-in (vec char-map) [0 0])

(count (for [x (range (count (first char-map)))
             y (range (count char-map))
             :when (= "X" (get-in (vec char-map) [x y]))]
         [x y]))


(defn find-xmas [x y m]
  (->> [check-coord-up
        check-coord-down
        check-coord-left
        check-coord-right
        check-coord-top-right
        check-coord-bottom-right
        check-coord-bottom-left
        check-coord-top-left]
       (filter (fn [f]
                 (f x y m)))
       count))


(defn solve1 [input]
  (let [char-map (map (fn [x] (str/split x #"")) input)]
    (->> (for [x (range (count (first char-map)))
               y (range (count char-map))
               :when (= "X" (get-in (vec char-map) [x y]))]
           [x y])
         (map (fn [[x y]] (find-xmas x y (vec char-map))))
         (reduce +))))

(solve1 (str/split-lines sample-input))

(solve1 (str/split-lines (read-single-line "resources/day04/input.txt")))