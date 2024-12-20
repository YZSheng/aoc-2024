(ns aoc2024.day20.solution
  (:require
   [aoc2024.core :refer [find-shortest-path]]
   [clojure.string :as str]))


(def sample-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
")

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (mapv vec lines)))

(parse-input sample-input)

(defn find-start [parsed-map]
  (first (for [x (range (count parsed-map))
               y (range (count (first parsed-map)))
               :when (= \S (get-in parsed-map [x y]))]
           [x y])))

(defn find-end [parsed-map]
  (first (for [x (range (count parsed-map))
               y (range (count (first parsed-map)))
               :when (= \E (get-in parsed-map [x y]))]
           [x y])))

(find-start (parse-input sample-input))
(find-end (parse-input sample-input))

(defn find-walls [parsed-map]
  (for [x (range (count parsed-map))
        y (range (count (first parsed-map)))
        :when (= \# (get-in parsed-map [x y]))]
    [x y]))

(find-walls (parse-input sample-input))

(find-shortest-path [0 0] [1 1] #{[0 1] [1 0]} 1 1)

(defn calculate-distances [parsed-map]
  (let [max-x (count parsed-map)
        max-y (count (first parsed-map))
        start (find-start parsed-map)
        walls (set (find-walls parsed-map))
        ;; Create a map of position -> distance from start
        distances (loop [to-visit (conj clojure.lang.PersistentQueue/EMPTY [start 0])
                        seen #{start}
                        result {start 0}]
          (if (empty? to-visit)
            result
            (let [[pos steps] (peek to-visit)
                  neighbors (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                :let [new-x (+ (first pos) dx)
                                      new-y (+ (second pos) dy)
                                      new-pos [new-x new-y]]
                                :when (and (>= new-x 0) (< new-x max-x)
                                         (>= new-y 0) (< new-y max-y)
                                         (not (walls new-pos))
                                         (not (seen new-pos)))]
                            new-pos)]
              (recur (into (pop to-visit) 
                          (map #(vector % (inc steps)) neighbors))
                     (into seen neighbors)
                     (into result (map #(vector % (inc steps)) neighbors))))))]
    distances))


(defn find-adjacent-spaces [x y max-x max-y distances]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [space-x (+ x dx)
              space-y (+ y dy)
              space-pos [space-x space-y]]
        :when (and (>= space-x 0) (< space-x max-x)
                   (>= space-y 0) (< space-y max-y)
                   (distances space-pos))]
    space-pos))

(defn find-shortcut-savings [parsed-map distances]
  (let [max-x (count parsed-map)
        max-y (count (first parsed-map))
        walls (set (find-walls parsed-map))]
    (for [wall-pos walls
          :let [[x y] wall-pos
                adjacent-spaces (find-adjacent-spaces x y max-x max-y distances)]
          :when (>= (count adjacent-spaces) 2)
          :let [spaces-by-dist (sort-by distances adjacent-spaces)
                before-space (first spaces-by-dist)
                after-space (last spaces-by-dist)
                saving (- (distances after-space) 
                         (distances before-space) 
                         2)]] ; extra 2 steps to move through the wall
      [wall-pos saving adjacent-spaces])))

(defn solve1 [input min-saving]
  (let [parsed-map (parse-input input)
        distances (calculate-distances parsed-map)
        shortcuts (find-shortcut-savings parsed-map distances)]
    (->> shortcuts
         (filter #(>= (second %) min-saving))
         (count))))

(solve1 sample-input 10)
(solve1 sample-input 64)
(solve1 sample-input 65)
(solve1 (slurp "resources/day20/input.txt") 100)