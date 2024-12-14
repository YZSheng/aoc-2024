(ns aoc2024.day14.solution
  (:require [clojure.string :as str]))

(def sample-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"-?\d+" %))
       (map #(map (fn [s] (Integer/parseInt s)) %))
       (map (fn [[x y vx vy]] {:position [x y] :velocity [vx vy]}))))

(defn print-positions [positions max-x max-y]
  (let [position-set (set positions)]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (print (if (position-set [x y]) "#" " ")))
      (println))))

(defn move-once [robots max-x max-y n]
  (let [result (map (fn [robot]
                      (let [new-x (mod (+ (first (:position robot)) (first (:velocity robot))) max-x)
                            new-y (mod (+ (last (:position robot)) (last (:velocity robot))) max-y)]
                        (assoc robot :position [new-x new-y])))
                    robots)]
    (when (or (= (mod (- n 134) 103) 0)  ; First pattern: 134, 237, 340, ...
              (= (mod (- n 173) 101) 0))  ; Second pattern: 173, 274, 375, ...
      (println "\n\n\n\n\nRuns : " n)
      (print-positions (map :position result) max-x max-y))
    result))

(defn move-n-times [robots n-times max-x max-y]
  (loop [robots robots
         n n-times]
    (if (zero? n)
      robots
      (recur (move-once robots max-x max-y (inc (- n-times n))) (dec n)))))

(defn group-by-quadrant [positions width height]
  (let [mid-x (quot width 2)
        mid-y (quot height 2)
        quadrants (group-by (fn [[x y]]
                              (cond
                                (and (< x mid-x) (> y mid-y)) :top-left
                                (and (> x mid-x) (> y mid-y)) :top-right
                                (and (< x mid-x) (< y mid-y)) :bottom-left
                                (and (> x mid-x) (< y mid-y)) :bottom-right
                                :else nil))
                            positions)]
    {:top-left     (count (get quadrants :top-left []))
     :top-right    (count (get quadrants :top-right []))
     :bottom-left  (count (get quadrants :bottom-left []))
     :bottom-right (count (get quadrants :bottom-right []))}))

(comment
  (parse-input sample-input)
  (print-positions [[3 1] [5 0] [9 5] [4 6] [1 3] [1 5] [6 3] [2 5] [0 6] [6 5] [4 1] [6 2]] 11 7)
  (group-by-quadrant [[3 5] [5 4] [9 0] [4 5] [1 6] [1 3] [6 0]
                      [2 3] [0 2] [6 0] [4 5] [6 6]] 11 7)
  ;; => {:top-left 1, :top-right 3, :bottom-left 4, :bottom-right 1}
  (move-n-times (parse-input "p=2,4 v=2,-3") 4 11 7)

  (map :position (move-n-times (parse-input sample-input) 100 11 7))

  (->> sample-input
       (parse-input)
       ((fn [robots] (move-n-times robots 1 11 7)))
       (map :position)
       ((fn [positions] (group-by-quadrant positions 11 7)))
       (vals)
       (apply *))

  (->> (slurp "resources/day14/input.txt")
       (parse-input)
       ((fn [robots] (move-n-times robots 100 101 103)))
       (map :position)
       ((fn [positions] (group-by-quadrant positions 101 103)))
       (vals)
       (apply *)))

(defn solve1 [input max-x max-y max-count]
  (->> input
       (parse-input)
       ((fn [robots] (move-n-times robots max-count max-x max-y)))
       (map :position)
       ((fn [positions] (group-by-quadrant positions max-x max-y)))
       (vals)
       (apply *)))

(solve1 (slurp "resources/day14/input.txt") 101 103 100)
;; part 2
(solve1 (slurp "resources/day14/input.txt") 101 103 10000)
