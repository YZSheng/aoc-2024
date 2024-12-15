(ns aoc2024.day15.solution
  (:require [clojure.string :as str]))

(def sample-input "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(def small-sample-input "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defn parse-input [input]
  (let [[map-input movement-input] (str/split input #"\n\n")]
    {:map (mapv vec (str/split-lines map-input))
     :movement (mapcat (fn [line] (str/split line #"")) (str/split-lines movement-input))}))

(parse-input small-sample-input)
(parse-input sample-input)

(def sample-map  [[\# \# \# \# \# \# \# \# \# \#]
                  [\# \. \. \O \. \. \O \. \O \#]
                  [\# \. \. \. \. \. \. \O \. \#]
                  [\# \. \O \O \. \. \O \. \O \#]
                  [\# \. \. \O \@ \. \. \O \. \#]
                  [\# \O \# \. \. \O \. \. \. \#]
                  [\# \O \. \. \O \. \. \O \. \#]
                  [\# \. \O \O \. \O \. \O \O \#]
                  [\# \. \. \. \. \O \. \. \. \#]
                  [\# \# \# \# \# \# \# \# \# \#]])

sample-map
(defn find-robot [m]
  (for [row (range (count m))
        cell (range (count (first m)))
        :when (= (get-in m [row cell]) \@)]
    [row cell]))

(find-robot sample-map)

(find-robot [[\# \# \# \# \# \# \# \# \# \#]
             [\# \. \. \O \. \. \O \. \O \#]
             [\# \. \. \. \. \. \. \O \. \#]
             [\# \. \O \O \. \. \O \. \O \#]
             [\# \. \. \O \. \@ \. \O \. \#]
             [\# \O \# \. \. \O \. \. \. \#]
             [\# \O \. \. \O \. \. \O \. \#]
             [\# \. \O \O \. \O \. \O \O \#]
             [\# \. \. \. \. \O \. \. \. \#]
             [\# \# \# \# \# \# \# \# \# \#]])

(defn get-connected-rocks [m pos direction]
  (loop [current-pos pos
         rocks []]
    (let [current-cell (get-in m current-pos)]
      (cond
        (nil? current-cell) nil
        (= current-cell \#) nil
        (= current-cell \O) (recur (case direction
                                     "v" (update current-pos 0 inc)
                                     "^" (update current-pos 0 dec)
                                     "<" (update current-pos 1 dec)
                                     ">" (update current-pos 1 inc))
                                   (conj rocks current-pos))
        (= current-cell \.) (when (seq rocks) rocks)
        :else nil))))

(defn move-robot [m robot-pos direction]
  (let [next-pos (case direction
                   "v" (update robot-pos 0 inc)
                   "^" (update robot-pos 0 dec)
                   "<" (update robot-pos 1 dec)
                   ">" (update robot-pos 1 inc))
        next-cell (get-in m next-pos)]
    (cond
      ;; Hit a wall
      (= next-cell \#) m

      ;; Empty space
      (= next-cell \.) (-> m
                           (assoc-in robot-pos \.)
                           (assoc-in next-pos \@))

      ;; Hit a rock
      (= next-cell \O)
      (if-let [rocks (get-connected-rocks m next-pos direction)]
        ;; Can move the chain of rocks
        (-> m
            (assoc-in robot-pos \.)  ; clear robot's old position
            ;; Clear all rock positions
            (as-> m' (reduce #(assoc-in %1 %2 \.) m' rocks))
            ;; Move all rocks one space in the direction
            (as-> m' (reduce #(assoc-in %1
                                        (case direction
                                          "v" (update %2 0 inc)
                                          "^" (update %2 0 dec)
                                          "<" (update %2 1 dec)
                                          ">" (update %2 1 inc))
                                        \O)
                             m'
                             rocks))
            (assoc-in next-pos \@))  ; put robot in new position
        ;; Can't move the chain
        m))))

sample-map

(move-robot sample-map [4 4] "v")
(move-robot sample-map [4 4] "<")

(find-robot [[\# \# \# \# \# \# \# \# \# \#]
             [\# \. \. \O \. \. \O \. \O \#]
             [\# \. \. \. \. \. \. \O \. \#]
             [\# \. \O \O \. \. \O \. \O \#]
             [\# \. \. \O \. \. \. \O \. \#]
             [\# \O \# \@ \. \O \. \. \. \#]
             [\# \O \. \. \O \. \. \O \. \#]
             [\# \. \O \O \. \O \. \O \O \#]
             [\# \. \. \. \. \O \. \. \. \#]
             [\# \# \# \# \# \# \# \# \# \#]])

(move-robot [[\# \# \# \# \# \# \# \# \# \#]
             [\# \. \. \O \. \. \O \. \O \#]
             [\# \. \. \. \. \. \. \O \. \#]
             [\# \. \O \O \. \. \O \. \O \#]
             [\# \. \. \O \. \. \. \O \. \#]
             [\# \O \# \@ \. \O \. \. \. \#]
             [\# \O \. \. \O \. \. \O \. \#]
             [\# \. \O \O \. \O \. \O \O \#]
             [\# \. \. \. \. \O \. \. \. \#]
             [\# \# \# \# \# \# \# \# \# \#]] [5 3] "^")

(str/split "<^^>>>vv<v>>v<<" #"")

small-sample-input
(parse-input small-sample-input)

(let [parsed (parse-input small-sample-input)
      parsed-map (:map parsed)
      parsed-movement (:movement parsed)]
  (reduce (fn [{:keys [m pos]} direction]
            (let [new-m (move-robot m pos direction)]
              {:m new-m
               :pos (first (find-robot new-m))}))
          {:m parsed-map
           :pos (first (find-robot parsed-map))}
          parsed-movement))

(defn find-boxes [m]
  (for [row (range (count m))
        cell (range (count (first m)))
        :when (= (get-in m [row cell]) \O)]
    [row cell]))

(defn solve1 [input]
  (let [parsed (parse-input input)
        parsed-map (:map parsed)
        parsed-movement (:movement parsed)
        result (reduce (fn [{:keys [m pos]} direction]
                         (let [new-m (move-robot m pos direction)]
                           {:m new-m
                            :pos (first (find-robot new-m))}))
                       {:m parsed-map
                        :pos (first (find-robot parsed-map))}
                       parsed-movement)
        boxes (find-boxes (:m result))]
    (->> boxes
         (map (fn [[x y]] (+ (* 100 x) y)))
         (reduce +))))

(solve1 small-sample-input)
(solve1 sample-input)
(solve1 (slurp "resources/day15/input.txt"))