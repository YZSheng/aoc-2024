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

(parse-input sample-input)

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



(find-frequencies sample-input)
(group-positions-by-frequency (find-frequencies sample-input))

(all-combinations [[1 8] [2 5] [3 7] [4 4]])

(defn get-next-antenna [[[x1 y1] [x2 y2]]]
  [(+ x2 (- x2 x1))
   (+ y2 (- y2 y1))])

(map get-next-antenna (all-combinations [[1 8] [2 5] [3 7] [4 4]]))

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

(get-all-antennas (parse-input sample-input) [[1 8] [2 5]])
(get-all-antennas (parse-input sample-input) [[1 1] [2 2]])
(get-all-antennas (parse-input sample-input) [[2 2] [1 1]])

(defn calculate-antennas [input]
  (let [parsed-input (parse-input input)
        frequencies (find-frequencies input)
        grouped (group-positions-by-frequency frequencies)]
    (->> grouped
         vals
         (map (fn [positions]
                (map get-next-antenna (all-combinations positions))))
         (apply concat)
         (distinct)
         (filter (fn [antenna]
                   (is-in-bounds? parsed-input antenna)))
         count)))

(calculate-antennas sample-input)

(calculate-antennas (slurp "resources/day08/input.txt"))


;; part 2

(defn find-antennas-for-positions [parsed-input positions]
  (let [combinations (all-combinations positions)]
    (->> combinations
         (map (fn [combination]
                (get-all-antennas parsed-input combination)))
         (apply concat)
         distinct)))

(find-antennas-for-positions (parse-input sample-input) [[1 8] [2 5] [3 7] [4 4]])

(defn calculate-antennas-2 [input]
  (let [parsed-input (parse-input input)
        frequencies (find-frequencies input)
        grouped (group-positions-by-frequency frequencies)
        frequency-positions (map :position frequencies)]
    (->> grouped
         vals
         (map (fn [positions]
                (find-antennas-for-positions parsed-input positions)))
         (apply concat frequency-positions)
         distinct
         count)))


(map :position (find-frequencies sample-input))

(calculate-antennas-2 sample-input)

(calculate-antennas-2 (slurp "resources/day08/input.txt"))

(def test-input "T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........")

(calculate-antennas-2 test-input)