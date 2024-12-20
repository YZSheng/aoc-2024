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
                          2)]]
      [[before-space after-space] saving])))

(defn solve1 [input min-saving]
  (let [parsed-map (parse-input input)
        distances (calculate-distances parsed-map)
        shortcuts (find-shortcut-savings parsed-map distances)]
    (->> shortcuts
         (filter #(>= (second %) min-saving))
         count)))

(solve1 sample-input 10)
(solve1 sample-input 64)
(solve1 sample-input 65)
(solve1 (slurp "resources/day20/input.txt") 100)


;; part 2

(defn get-neighbors [max-x max-y parsed-map x y]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [nx (+ x dx)
              ny (+ y dy)]
        :when (and (< nx max-x) (>= nx 0)
                   (< ny max-y) (>= ny 0)
                   (not= \# (get-in parsed-map [ny nx])))]
    [[nx ny] 1]))

(defn build-graph [parsed-map]
  (let [max-y (count parsed-map)
        max-x (count (first parsed-map))]
    (into {}
          (for [y (range 1 (dec max-y))
                x (range 1 (dec max-x))
                :when (not= \# (get-in parsed-map [y x]))
                :let [neighbors (into {} (get-neighbors max-x max-y parsed-map x y))]]
            [[x y] neighbors]))))

(defn dijkstra [graph start]
  (loop [distances {start 0}
         queue (sorted-set [0 start])]
    (if (empty? queue)
      distances
      (let [[curr-dist curr-pos] (first queue)
            queue (disj queue [curr-dist curr-pos])
            neighbors (get graph curr-pos)]
        (if (nil? neighbors)
          (recur distances queue)
          (let [updates (for [[next-pos edge-weight] neighbors
                              :let [new-dist (+ curr-dist edge-weight)]
                              :when (or (not (distances next-pos))
                                        (< new-dist (distances next-pos)))]
                          [next-pos new-dist])
                new-distances (into distances updates)
                new-queue (into queue (map (fn [[pos dist]] [dist pos]) updates))]
            (recur new-distances new-queue)))))))

(defn create-distance-map [parsed-map distances]
  (let [max-x (count parsed-map)
        max-y (count (first parsed-map))]
    (vec (for [y (range max-x)]
           (vec (for [x (range max-y)]
                  (if (= \# (get-in parsed-map [y x]))
                    Double/POSITIVE_INFINITY
                    (get distances [x y] Double/POSITIVE_INFINITY))))))))

(defn solve2 [input min-saving]
  (let [parsed-map (parse-input input)
        max-x (count parsed-map)
        max-y (count (first parsed-map))
        [end-x end-y] (find-end parsed-map)
        graph (build-graph parsed-map)
        distances (dijkstra graph [end-x end-y])
        path-distance-map (create-distance-map parsed-map distances)
        max-steps 20]
    (->> (for [x (range 1 (dec max-x))
               y (range 1 (dec max-y))
               :let [d (get-in path-distance-map [x y])]
               :when (not= d Double/POSITIVE_INFINITY)
               x2 (range (max 0 (- x max-steps))
                         (inc (min (dec max-x) (+ x max-steps))))
               y2 (range (max 0 (- y (- max-steps (abs (- x x2)))))
                         (inc (min (dec max-y) (+ y (- max-steps (abs (- x x2)))))))
               :let [change-distance (get-in path-distance-map [x2 y2])
                     manhattan-distance (+ (abs (- x x2)) (abs (- y y2)))]
               :when (and (not= change-distance Double/POSITIVE_INFINITY)
                          (>= (- d change-distance manhattan-distance) min-saving))]
           [x y x2 y2])
         count)))

(solve2 sample-input 76)
(solve2 (slurp "resources/day20/input.txt") 100)