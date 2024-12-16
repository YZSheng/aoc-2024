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

(defn get-new-moves [next-moves visited unvisited]
  (into {}
        (for [[k v] next-moves
              :when (not (visited k))
              :let [existing (get unvisited k)]
              :when (or (nil? existing)
                        (< (:cost v) (:cost existing)))]
          [k v])))

(defn find-paths [parsed-map start-pos end-pos initial-direction]
  (let [initial-state {:pos start-pos
                       :direction initial-direction
                       :cost 0
                       :path []}]
    (loop [unvisited {[start-pos initial-direction] initial-state}
           visited #{}
           current-state initial-state]
      (let [{:keys [pos direction cost path]} current-state]

        (cond
          (= pos end-pos)
          [path]

          (empty? unvisited)
          []

          :else
          (let [forward-pos (get-next-position pos direction)
                forward-move (when (valid-move? parsed-map forward-pos)
                               {[forward-pos direction]
                                {:pos forward-pos
                                 :direction direction
                                 :cost (+ cost 1)
                                 :path (conj path :forward)}})

                left-dir (rotate-left direction)
                left-move {[pos left-dir]
                           {:pos pos
                            :direction left-dir
                            :cost (+ cost 1000)
                            :path (conj path :rotate-left)}}

                right-dir (rotate-right direction)
                right-move {[pos right-dir]
                            {:pos pos
                             :direction right-dir
                             :cost (+ cost 1000)
                             :path (conj path :rotate-right)}}

                next-moves (merge forward-move left-move right-move)
                new-moves (get-new-moves next-moves visited unvisited)
                updated-unvisited (-> unvisited
                                      (dissoc [pos direction])
                                      (merge new-moves))

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


;; part 2
(defn find-paths-2 [parsed-map start-pos end-pos initial-direction]
  (let [initial-state {:pos start-pos
                       :direction initial-direction
                       :cost 0
                       :path []}]
    (loop [unvisited {[start-pos initial-direction] initial-state} 
           visited #{}
           paths []
           min-cost Integer/MAX_VALUE]
      (if (empty? unvisited)
        paths
        (let [[current-key current-state] (apply min-key #(:cost (val %)) unvisited)
              {:keys [pos direction cost path]} current-state]
          (cond
            (and (= pos end-pos) (<= cost min-cost))
            (if (= cost min-cost)
              (recur (dissoc unvisited current-key)
                     (conj visited current-key)
                     (conj paths path)
                     min-cost)
              (recur (dissoc unvisited current-key)
                     (conj visited current-key)
                     [path]
                     cost))

            (or (> cost min-cost) (visited current-key))
            (recur (dissoc unvisited current-key)
                   visited
                   paths
                   min-cost)

            :else
            (let [forward-pos (get-next-position pos direction)
                  forward-move (when (valid-move? parsed-map forward-pos)
                                {[forward-pos direction]
                                 {:pos forward-pos
                                  :direction direction
                                  :cost (+ cost 1)
                                  :path (conj path {:move :forward :position forward-pos})}})

                  left-dir (rotate-left direction)
                  left-move {[pos left-dir]
                            {:pos pos
                             :direction left-dir
                             :cost (+ cost 1000)
                             :path (conj path {:move :rotate-left :position pos})}}

                  right-dir (rotate-right direction)
                  right-move {[pos right-dir]
                             {:pos pos
                              :direction right-dir
                              :cost (+ cost 1000)
                              :path (conj path {:move :rotate-right :position pos})}}

                  next-moves (merge forward-move left-move right-move)
                  new-moves (get-new-moves next-moves visited unvisited)
                  updated-unvisited (-> unvisited
                                      (dissoc current-key)
                                      (merge new-moves))]

              (recur updated-unvisited
                     (conj visited current-key)
                     paths
                     min-cost))))))))

(defn find-all-moves-2 [input]
  (let [parsed (vec (parse-input input))
        start-pos (first (find-reindeer-position input))
        end-pos (first (find-end-position input))]
    (find-paths-2 parsed start-pos end-pos :east)))


(find-all-moves-2 "####
#..#
#SE#
####")

(find-all-moves-2 "####
#.E#
#S.#
####")

(find-all-moves-2 "####
#.E#
#S##
####")

(find-all-moves-2 "#####
#..E#
#S..#
#####")

(count (find-all-moves-2 sample-input))

(defn solve2 [input]
  (->> input
       (find-all-moves-2)
       first
       :visited
       (count)
       (+ 1)))

(solve2 "#####
#..E#
#S..#
#####")

(solve2 "#################
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

(solve2 (slurp "resources/day16/input.txt"))