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

(defn find-path-from-distances [parsed-map distances]
  (let [end (find-end parsed-map)
        start (find-start parsed-map)
        dirs [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [current end
           path [end]]
      (if (= current start)
        (reverse path)
        (let [current-dist (distances current)
              next-pos (first
                        (for [[dx dy] dirs
                              :let [nx (+ (first current) dx)
                                    ny (+ (second current) dy)
                                    next-pos [nx ny]]
                              :when (and (distances next-pos)
                                         (= (distances next-pos)
                                            (dec current-dist)))]
                          next-pos))]
          (recur next-pos (conj path next-pos)))))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn calculate-distance-from-end [path]
  (into {} (map-indexed (fn [idx pos] [pos (- (count path) idx 1)]) path)))

(defn solve2 [input min-saving]
  (let [parsed-map (parse-input input)
        distances (calculate-distances parsed-map)
        path (find-path-from-distances parsed-map distances)
        path-distances (calculate-distance-from-end path)
        max-steps 20]
    (->> (for [pos path
               other-pos path
               :let [pos-dist (path-distances pos)
                     other-dist (path-distances other-pos)
                     md (manhattan-distance pos other-pos)]
               :when (and (< other-dist pos-dist)
                          (<= md max-steps)
                          (>= (- pos-dist (+ other-dist md)) min-saving))]
           [pos other-pos])
         count)))

(solve2 sample-input 76)
(solve2 sample-input 74)
(solve2 (slurp "resources/day20/input.txt") 100)