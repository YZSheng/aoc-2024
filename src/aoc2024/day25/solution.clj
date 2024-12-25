(ns aoc2024.day25.solution
  (:require
   [aoc2024.core :refer [transpose]]
   [clojure.string :as str]))

(def sample-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(defn parse-key-lock [input]
  (->> input
       (str/split-lines)
       (mapv #(str/split % #""))
       transpose
       (mapv (fn [chars] (dec (count (filter #(= "#" %) chars)))))))

(defn parse-input [input]
  (->> input
       (#(str/split % #"\n\n"))
       (group-by (fn [schematic] (first (str/split schematic #"\n"))))
       ((fn [group]
          {:keys (map parse-key-lock (group "....."))
           :locks (map parse-key-lock (group "#####"))}))))

(defn solve1 [input]
  (let [{:keys [keys locks]} (parse-input input)]
    (count (for [l locks
                 k keys
                 :when (every? #(< % 6) (map + k l))]
             [l k]))))

(solve1 sample-input)
(solve1 (slurp "resources/day25/input.txt"))
