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

(defn valid-pos? [grid pos]
  (and (>= (first pos) 0)
       (>= (second pos) 0)
       (< (first pos) (count grid))
       (< (second pos) (count (first grid)))))

(defn walk [grid start-pos start-dir obstacle]
  (let [next-dir {"^" ">"
                  ">" "v"
                  "v" "<"
                  "<" "^"}]
    (loop [pos start-pos
           dir start-dir
           seen #{pos}
           seen-with-dir #{[pos dir]}]
      (let [next-pos (get-next-pos pos dir)]
        (cond
          (seen-with-dir [next-pos dir])
          #{}
          
          (not (valid-pos? grid next-pos))
          seen
          
          (or (= "#" (get-in grid (reverse next-pos)))
              (= next-pos obstacle))
          (recur pos 
                 (next-dir dir)
                 seen
                 (conj seen-with-dir [pos (next-dir dir)]))
          
          :else
          (recur next-pos
                 dir
                 (conj seen next-pos)
                 (conj seen-with-dir [next-pos dir])))))))

(defn place-obstructions [input]
  (let [grid (parse-input input)
        [x y direction] (find-guard input)
        initial-points (walk grid [x y] direction nil)]
    (->> initial-points
         (filter #(not= [x y] %))  
         (filter #(not= "#" (get-in grid (reverse %))))  
         (filter #(empty? (walk grid [x y] direction %)))
         set)))

(count (place-obstructions sample-input))
(count (place-obstructions (slurp "resources/day06/input.txt")))
