(ns aoc2024.day08.solution
  (:require
   [clojure.string :as str]))

(def sample-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn parse-input [input]
  (mapv #(str/split % #"") (str/split-lines input)))

(defn find-frequencies [input]
  (let [parsed-input (parse-input input)]
    (for [x (range 0 (count parsed-input))
          y (range 0 (count (first parsed-input)))
          :when (not= (get-in parsed-input [x y]) ".")]
      {:frequency (get-in parsed-input [x y])
       :position [x y]})))

(defn group-positions-by-frequency [data]
  (->> data
       (group-by :frequency)
       (map (fn [[freq entries]]
              [freq (map :position entries)]))
       (into {})))

(defn all-combinations [coll]
  (for [x coll
        y coll
        :when (not= x y)]
    [x y]))

(defn get-next-antenna [[[x1 y1] [x2 y2]]]
  [(+ x2 (- x2 x1))
   (+ y2 (- y2 y1))])

(defn is-in-bounds? [parsed-input [x y]]
  (and (< -1 x (count parsed-input))
       (< -1 y (count (first parsed-input)))))

(defn get-all-antennas [parsed-input [[x1 y1] [x2 y2]]]
  (loop [result []
         multiplier 1]
    (let [next-antenna [(+ x2 (* multiplier (- x2 x1)))
                        (+ y2 (* multiplier (- y2 y1)))]]
      (if (is-in-bounds? parsed-input next-antenna)
        (recur (conj result next-antenna) (inc multiplier))
        result))))

(defn solve1 [input]
  (let [parsed-input (parse-input input)
        frequencies (find-frequencies input)
        grouped (group-positions-by-frequency frequencies)]
    (->> grouped
         vals
         (map (fn [positions]
                (map get-next-antenna (all-combinations positions))))
         (apply concat)
         (distinct)
         (filter (partial is-in-bounds? parsed-input))
         count)))

(solve1 sample-input)

(solve1 (slurp "resources/day08/input.txt"))


;; part 2

(defn find-antennas-for-positions [parsed-input positions]
  (let [combinations (all-combinations positions)]
    (->> combinations
         (map (fn [combination]
                (get-all-antennas parsed-input combination)))
         (apply concat)
         distinct)))

(defn solve2 [input]
  (let [parsed-input (parse-input input)
        frequencies (find-frequencies input)
        grouped (group-positions-by-frequency frequencies)
        frequency-positions (map :position frequencies)]
    (->> grouped
         vals
         (map (partial find-antennas-for-positions parsed-input))
         (apply concat frequency-positions)
         distinct
         count)))

(solve2 sample-input)

(solve2 (slurp "resources/day08/input.txt"))
