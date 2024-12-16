(ns aoc2024.day16.solution
  (:require
   [clojure.string :as str]))

(def sample-input "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
")

(str/blank? sample-input)

(defn parse-input [input]
  (->> (str/split-lines input)
       (map vec)))

(parse-input sample-input)

(defn find-reindeer-position [input]
  (let [parsed (vec (parse-input input))]
    (for [x (range (count parsed))
          y (range (count (first parsed)))
          :when (= \S (get-in parsed [x y]))]
      [x y])))

(find-reindeer-position sample-input)

(defn find-end-position [input]
  (let [parsed (vec (parse-input input))]
    (for [x (range (count parsed))
          y (range (count (first parsed)))
          :when (= \E (get-in parsed [x y]))]
      [x y])))

(find-end-position sample-input)

(defn get-next-position [[x y] direction]
  (case direction
    :north [(dec x) y]
    :south [(inc x) y]
    :east [x (inc y)]
    :west [x (dec y)]))

(defn rotate-left [direction]
  (case direction
    :north :west
    :west :south
    :south :east
    :east :north))

(defn rotate-right [direction]
  (case direction
    :north :east
    :east :south
    :south :west
    :west :north))

(defn valid-move? [parsed-map [x y]]
  (and (< -1 x (count parsed-map))
       (< -1 y (count (first parsed-map)))
       (not= \# (get-in parsed-map [x y]))))

(defn find-paths [parsed-map start-pos end-pos initial-direction]
  (let [initial-state {:pos start-pos
                       :direction initial-direction
                       :cost 0
                       :path []}]
    (loop [unvisited {[start-pos initial-direction] initial-state}  ; map of [pos direction] -> state
           visited #{}
           current-state initial-state]
      (let [{:keys [pos direction cost path]} current-state]

        (cond
          ;; Found the end
          (= pos end-pos)
          [path]

          ;; No more positions to visit
          (empty? unvisited)
          []

          :else
          ;; Generate all possible next moves
          (let [;; Try moving forward
                forward-pos (get-next-position pos direction)
                forward-move (when (valid-move? parsed-map forward-pos)
                               {[forward-pos direction]
                                {:pos forward-pos
                                 :direction direction
                                 :cost (+ cost 1)
                                 :path (conj path :forward)}})

                ;; Try rotating left
                left-dir (rotate-left direction)
                left-move {[pos left-dir]
                           {:pos pos
                            :direction left-dir
                            :cost (+ cost 1000)
                            :path (conj path :rotate-left)}}

                ;; Try rotating right
                right-dir (rotate-right direction)
                right-move {[pos right-dir]
                            {:pos pos
                             :direction right-dir
                             :cost (+ cost 1000)
                             :path (conj path :rotate-right)}}

                ;; Combine all possible moves
                next-moves (merge forward-move left-move right-move)

                ;; Filter out visited positions and update costs if better path found
                new-moves (into {}
                                (for [[k v] next-moves
                                      :when (not (visited k))
                                      :let [existing (get unvisited k)]
                                      :when (or (nil? existing)
                                                (< (:cost v) (:cost existing)))]
                                  [k v]))

                ;; Update unvisited with new moves
                updated-unvisited (-> unvisited
                                      (dissoc [pos direction])
                                      (merge new-moves))

                ;; Find next position to visit (lowest cost)
                next-state (apply min-key
                                  #(:cost (val %))
                                  updated-unvisited)]

            (recur (dissoc updated-unvisited (key next-state))
                   (conj visited [pos direction])
                   (val next-state))))))))

(defn find-all-moves [input]
  (let [parsed (vec (parse-input input))
        start-pos (first (find-reindeer-position input))
        end-pos (first (find-end-position input))]
    (find-paths parsed start-pos end-pos :east)))

(find-reindeer-position sample-input)
(find-end-position sample-input)

(find-paths (vec (parse-input sample-input)) [13 1] [1 13] :east)

(find-reindeer-position "####
#..#
#SE#
####")

(find-all-moves "####
#..#
#SE#
####")

(find-all-moves "####
#.E#
#S.#
####")

(find-all-moves "####
#.E#
#S##
####")

(find-all-moves sample-input)

(defn calculate-cost [path]
  (->> path
       (map (fn [move]
              (if (= :forward move) 1 1000)))
       (reduce +)))

(->> sample-input
     (find-all-moves)
     (map calculate-cost)
     (sort)
     first)

(defn solve1 [input]
  (->> input
       (find-all-moves)
       (map calculate-cost)
       (sort)))

(solve1 sample-input)
(solve1 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(solve1 (slurp "resources/day16/input.txt"))
