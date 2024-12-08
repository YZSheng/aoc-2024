(ns aoc2024.day09.solution
  (:require
   [clojure.string :as str]))

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse-input [input]
  (map #(str/split % #" ") (str/split-lines input)))

(parse-input sample-input)
