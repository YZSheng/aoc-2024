(ns aoc2024.day06.solution
  (:require [clojure.string :as str]))

(def sample-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")


(defn parse-input [input]
  (->> input
       (str/split-lines)
       (mapv (fn [line] (str/split line #"")))))



(parse-input sample-input)

(get-in (parse-input sample-input) [0 4])

(defn find-guard [input]
  (let [grid (parse-input input)]
    (->> (for [x (range (count grid))
               y (range (count (first grid)))]
           (case (get-in grid [y x])
             "v" [x y "v"]
             "^" [x y "^"]
             "<" [x y "<"]
             ">" [x y ">"]
             nil))
         (filter some?)
         first)))

(find-guard sample-input)

(defn move-guard [input]
  (let [grid (parse-input input)
        [x y direction] (find-guard input)]
    (loop [[x y] [x y]
           direction direction
           visited #{[x y]}]
      (if (or (and (= direction "<")
                   (= x 0))
              (and (= direction "^")
                   (= y 0))
              (and (= direction ">")
                   (= x (dec (count grid))))
              (and (= direction "v")
                   (= y (dec (count (first grid))))))
        visited
        (let [next-position (case direction
                              "<" [(dec x) y]
                              "^" [x (dec y)]
                              ">" [(inc x) y]
                              "v" [x (inc y)])
              char-in-next-position (get-in grid (reverse next-position))]
          (if (= "#" char-in-next-position)
            (recur [x y]
                   (case direction
                     "^" ">"
                     ">" "v"
                     "<" "^"
                     "v" "<")
                   visited)
            (recur next-position
                   direction
                   (conj visited next-position))))))))

(count (move-guard sample-input))
(count (move-guard (slurp "resources/day06/input.txt")))

;; part 2
(defn get-next-pos [[x y] dir]
  (case dir
    "<" [(dec x) y]
    "^" [x (dec y)]
    ">" [(inc x) y]
    "v" [x (inc y)]))

(defn turn-right [dir]
  (case dir
    "^" ">"
    ">" "v"
    "v" "<"
    "<" "^"))

(defn valid-pos? [grid pos]
  (and (>= (first pos) 0)
       (>= (second pos) 0)
       (< (first pos) (count grid))
       (< (second pos) (count (first grid)))))

(defn will-run-forever? [grid start-pos start-dir obstruction max-steps]
  (loop [pos start-pos
         dir start-dir
         steps 0]
    (if (> steps max-steps)
      true
      (let [next-pos (get-next-pos pos dir)]
        (cond
          (not (valid-pos? grid next-pos))
          false

          (= next-pos obstruction)
          (recur pos (turn-right dir) (inc steps))

          (= "#" (get-in grid (reverse next-pos)))  ; Hit existing wall
          (recur pos (turn-right dir) (inc steps))

          :else
          (recur next-pos dir (inc steps)))))))

(defn place-obstructions [input]
  (let [grid (parse-input input)
        [x y direction] (find-guard input)]
    (loop [[x y] [x y]
           direction direction
           visited-with-direction #{[[x y] direction]}
           obstructions #{}]
      (if (not (valid-pos? grid (get-next-pos [x y] direction)))
        obstructions
        (let [next-position (get-next-pos [x y] direction)
              char-in-next-position (get-in grid (reverse next-position))]
          (if (= "#" char-in-next-position)
            (recur [x y]
                   (turn-right direction)
                   (conj visited-with-direction [[x y] (turn-right direction)])
                   obstructions)
            (let [should-obstruct? (and (not= "#" char-in-next-position)
                                        (will-run-forever? grid
                                                           [x y]
                                                           direction
                                                           next-position
                                                           10000))]
              (recur next-position
                     direction
                     (conj visited-with-direction [next-position direction])
                     (if should-obstruct?
                       (conj obstructions next-position)
                       obstructions)))))))))
;; Test it
(count (place-obstructions sample-input))
(count (place-obstructions (slurp "resources/day06/input.txt")))
