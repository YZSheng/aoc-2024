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

(defn find-robot [m]
  (for [row (range (count m))
        cell (range (count (first m)))
        :when (= (get-in m [row cell]) \@)]
    [row cell]))

(comment

  sample-map
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
               [\# \# \# \# \# \# \# \# \# \#]]))

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
      (= next-cell \#) m

      (= next-cell \.) (-> m
                           (assoc-in robot-pos \.)
                           (assoc-in next-pos \@))

      (= next-cell \O)
      (if-let [rocks (get-connected-rocks m next-pos direction)]
        (-> m
            (assoc-in robot-pos \.) ; set robot position to .
            (as-> m' (reduce #(assoc-in %1 %2 \.) m' rocks)) ; set rocks to .
            (as-> m' (reduce #(assoc-in %1
                                        (case direction
                                          "v" (update %2 0 inc)
                                          "^" (update %2 0 dec)
                                          "<" (update %2 1 dec)
                                          ">" (update %2 1 inc))
                                        \O)
                             m'
                             rocks)) ; move \O to the new position
            (assoc-in next-pos \@)) ; update robot new position
        m))))

(comment
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
               [\# \# \# \# \# \# \# \# \# \#]] [5 3] "^"))

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

;; part 2

(parse-input "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(def part-2-sample-map  [[\# \# \# \# \# \# \#]
                         [\# \. \. \. \# \. \#]
                         [\# \. \. \. \. \. \#]
                         [\# \. \. \O \O \@ \#]
                         [\# \. \. \O \. \. \#]
                         [\# \. \. \. \. \. \#]
                         [\# \# \# \# \# \# \#]])

(defn scale-map [m]
  (if (string? m)
    (scale-map (mapv vec (str/split-lines m)))
    (mapv (fn [row]
            (vec (mapcat (fn [cell]
                           (case cell
                             \@ [\@ \.]
                             \. [\. \.]
                             \# [\# \#]
                             \O [\[ \]]))
                         row)))
          m)))

(def scaled-part-2-sample-map (scale-map part-2-sample-map))

(defn get-next-positions [pos direction]
  (case direction
    "v" (update pos 0 inc)
    "^" (update pos 0 dec)
    "<" (update pos 1 dec)
    ">" (update pos 1 inc)))

(defn is-block? [cell]
  (contains? #{\[ \]} cell))

(defn find-connected-blocks-move-vertically [next-pos m direction]
  (loop [to-check #{next-pos}
         blocks #{}]
    (if (empty? to-check)
      (vec blocks)
      (let [check-pos (first to-check)
            rest-to-check (disj to-check check-pos)
            cell (get-in m check-pos)]
        (if (is-block? cell)
          (let [current-blocks (if (= cell \[)
                                 [check-pos
                                  [(first check-pos) (inc (second check-pos))]]
                                 [check-pos
                                  [(first check-pos) (dec (second check-pos))]])
                next-row ((if (= direction "^") dec inc) (first check-pos))
                next-positions [[next-row (second (first current-blocks))]
                                [next-row (second (second current-blocks))]]]
            (recur (into rest-to-check
                         (filter #(is-block? (get-in m %)) next-positions))
                   (into blocks current-blocks)))
          (recur rest-to-check blocks))))))

(defn find-connected-blocks-move-horizontally [next-pos m direction]
  (loop [col (second next-pos)
         blocks #{}]
    (let [cell (get-in m [(first next-pos) col])]
      (if (is-block? cell)
        (let [pair-col (if (= cell \[) (inc col) (dec col))]
          (recur ((if (= direction "<") dec inc) col)
                 (conj blocks [(first next-pos) col]
                       [(first next-pos) pair-col])))
        (vec blocks)))))

(defn get-connected-rocks-with-double-space-blocks [m pos direction]
  (let [next-pos (get-next-positions pos direction)
        current-cell (get-in m next-pos)]
    (when (is-block? current-cell)
      (if (#{"^" "v"} direction)
        (find-connected-blocks-move-vertically next-pos m direction)
        (find-connected-blocks-move-horizontally next-pos m direction)))))

(comment
  part-2-sample-map
  scaled-part-2-sample-map
  (find-robot scaled-part-2-sample-map)

  scaled-part-2-sample-map

  (find-robot scaled-part-2-sample-map)

  (get-connected-rocks-with-double-space-blocks scaled-part-2-sample-map (first (find-robot scaled-part-2-sample-map)) "<")

  (def part-2-connected-blocks-map [[\# \# \# \# \# \# \# \# \# \# \# \# \# \#]
                                    [\# \# \. \. \. \. \. \. \# \# \. \. \# \#]
                                    [\# \# \. \. \. \. \. \. \. \. \. \. \# \#]
                                    [\# \# \. \. \. \. \[ \] \[ \] \. \. \# \#]
                                    [\# \# \. \. \. \. \[ \] \. \. \. \. \# \#]
                                    [\# \# \. \. \. \. \. \@ \. \. \. \. \# \#]
                                    [\# \# \# \# \# \# \# \# \# \# \# \# \# \#]])

  part-2-connected-blocks-map
  (find-robot part-2-connected-blocks-map)
  (get-connected-rocks-with-double-space-blocks part-2-connected-blocks-map [5 7] "^")

  (def part-2-connected-complex-blocks-map [[\# \# \# \# \# \# \# \# \# \# \# \# \# \#]
                                            [\# \# \. \. \. \. \. \. \# \# \. \. \# \#]
                                            [\# \# \. \. \. \. \. \. \. \. \. \. \# \#]
                                            [\# \# \. \. \. \[ \] \[ \] \. \. \. \# \#]
                                            [\# \# \. \. \. \. \[ \] \. \. \. \. \# \#]
                                            [\# \# \. \. \. \. \. \@ \. \. \. \. \# \#]
                                            [\# \# \# \# \# \# \# \# \# \# \# \# \# \#]])

  part-2-connected-complex-blocks-map
  (find-robot part-2-connected-complex-blocks-map)
  (get-connected-rocks-with-double-space-blocks part-2-connected-complex-blocks-map [5 7] "^")

  (get-connected-rocks-with-double-space-blocks [[\# \# \# \# \# \# \# \# \# \# \# \# \# \#]
                                                 [\# \# \. \. \. \. \. \. \# \# \. \. \# \#]
                                                 [\# \# \. \. \. \[ \] \[ \] \. \. \. \# \#]
                                                 [\# \# \. \. \. \@ \[ \] \. \. \. \. \# \#]
                                                 [\# \# \. \. \. \. \. \. \. \. \. \. \# \#]
                                                 [\# \# \. \. \. \. \. \. \. \. \. \. \# \#]
                                                 [\# \# \# \# \# \# \# \# \# \# \# \# \# \#]] [3 5] "^"))

(defn valid-position? [m pos]
  (and pos
       (>= (first pos) 0)
       (>= (second pos) 0)
       (< (first pos) (count m))
       (< (second pos) (count (first m)))))

(defn get-new-rock-positions [m rocks direction]
  (map (fn [pos]
         (let [new-pos (case direction
                         "v" (update pos 0 inc)
                         "^" (update pos 0 dec)
                         "<" (update pos 1 dec)
                         ">" (update pos 1 inc))
               target-cell (get-in m new-pos)]
           (when (and (valid-position? m new-pos)
                      (not= target-cell \#))
             [pos new-pos])))
       rocks))

(defn move-all-connected-blocks [m robot-pos next-pos direction]
  (if-let [rocks (seq (get-connected-rocks-with-double-space-blocks m robot-pos direction))]
    (let [new-rock-positions (get-new-rock-positions m rocks direction)
          all-valid? (every? some? new-rock-positions)]
      (if all-valid?
        (-> m
            (assoc-in robot-pos \.)
            (as-> m' (reduce #(assoc-in %1 (first %2) \.) m' new-rock-positions))
            (as-> m' (reduce #(assoc-in %1 (second %2) (get-in m (first %2)))
                             m'
                             new-rock-positions))
            (assoc-in next-pos \@))
        m))
    m))

(defn move-robot-connected-blocks [m robot-pos direction]
  (let [next-pos (case direction
                   "v" (update robot-pos 0 inc)
                   "^" (update robot-pos 0 dec)
                   "<" (update robot-pos 1 dec)
                   ">" (update robot-pos 1 inc))
        next-cell (get-in m next-pos)]
    (cond
      (= next-cell \.)
      (-> m
          (assoc-in robot-pos \.)
          (assoc-in next-pos \@))

      (contains? #{\[ \]} next-cell)
      (move-all-connected-blocks m robot-pos next-pos direction)

      (= next-cell \#) m

      :else m)))

(defn find-boxes-left-bracket [m]
  (for [row (range (count m))
        cell (range (count (first m)))
        :when (= (get-in m [row cell]) \[)]
    [row cell]))

(comment
  (find-boxes-left-bracket part-2-connected-complex-blocks-map)
  (move-robot-connected-blocks part-2-connected-complex-blocks-map [5 7] "^")
  scaled-part-2-sample-map
  (find-robot scaled-part-2-sample-map)
  (move-robot-connected-blocks scaled-part-2-sample-map [3 10] "<"))

(defn solve2 [input]
  (let [parsed (parse-input input)
        parsed-map (scale-map (:map parsed))
        parsed-movement (:movement parsed)
        initial-pos (first (find-robot parsed-map))
        result (reduce (fn [{:keys [m pos]} direction]
                         (if (nil? pos)
                           {:m m :pos (first (find-robot m))}
                           (let [new-m (move-robot-connected-blocks m pos direction)
                                 new-pos (first (find-robot new-m))]
                             {:m new-m
                              :pos new-pos})))
                       {:m parsed-map
                        :pos initial-pos}
                       parsed-movement)

        boxes (find-boxes-left-bracket (:m result))]
    (->> boxes
         (map (fn [[x y]] (+ (* 100 x) y)))
         (reduce +))))

(solve2 small-sample-input)
(solve2 sample-input)
(solve2 (slurp "resources/day15/input.txt"))
