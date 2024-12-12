(ns aoc2024.day12.solution
  (:require [clojure.string :as str]))


(def sample-input "AAAA
BBCD
BBCC
EEEC")

(def sample-input-large "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv vec)))

(defn find-all-chars [input]
  (->> (parse-input input)
       (apply concat)
       (distinct)))

(find-all-chars sample-input)

(defn get-neighbors [parsed-input x y]
  (->> [[x (dec y)]
        [x (inc y)]
        [(dec x) y]
        [(inc x) y]]
       (map (fn [coords] {:coords coords
                          :char (get-in parsed-input coords)}))
       (filter (fn [neighbor] (some? (:char neighbor))))))

(get-neighbors (parse-input sample-input) 0 0)

(defn calculate-connected-area [input [x y]]
  (let [parsed-input (parse-input input)
        char (get-in parsed-input [x y])]
    (loop [to-visit #{[x y]}
           visited #{}]
      (if (empty? to-visit)
        (count visited)
        (let [current (first to-visit)
              neighbors (get-neighbors parsed-input (first current) (second current))
              neighbors-with-char (filter #(= (:char %) char) neighbors)
              neighbor-coords (map :coords neighbors-with-char)
              unvisited-neighbors (remove visited neighbor-coords)]
          (recur (into (disj to-visit current) unvisited-neighbors)
                 (conj visited current)))))))

(calculate-connected-area sample-input [0 0])
(calculate-connected-area sample-input [3 0])
(calculate-connected-area sample-input-large [0 0])


(defn find-area-coords [parsed-input [x y]]
  (loop [to-visit #{[x y]}
         area-visited #{}]
    (if (empty? to-visit)
      area-visited
      (let [current (first to-visit)
            char (get-in parsed-input current)
            neighbors (get-neighbors parsed-input (first current) (second current))
            neighbors-with-char (filter #(= (:char %) char) neighbors)
            neighbor-coords (map :coords neighbors-with-char)
            unvisited-neighbors (remove area-visited neighbor-coords)]
        (recur (into (disj to-visit current) unvisited-neighbors)
               (conj area-visited current))))))

(defn find-connected-areas [input]
  (let [parsed-input (parse-input input)
        height (count parsed-input)
        width (count (first parsed-input))]
    (loop [x 0
           y 0
           visited #{}
           areas []]
      (cond
        (>= x height) areas
        (>= y width) (recur (inc x) 0 visited areas)
        (visited [x y]) (recur x (inc y) visited areas)
        :else (let [area-coords (find-area-coords parsed-input [x y])]
                (recur x
                       (inc y)
                       (into visited area-coords)
                       (conj areas area-coords)))))))

(find-connected-areas sample-input)

(defn calculate-perimeter [area-coords]
  (let [coords (set area-coords)]
    (->> (for [coord coords
               :let [[x y] coord
                     neighbors [[x (dec y)]
                                [x (inc y)]
                                [(dec x) y]
                                [(inc x) y]]]]
           (count (remove coords neighbors)))
         (reduce +))))

(calculate-perimeter #{[0 0] [0 3] [0 2] [0 1]})
(calculate-perimeter #{[1 0] [1 1] [2 0] [2 1]})

(defn solve1 [input]
  (let [areas (find-connected-areas input)]
    (->> areas
         (map (fn [area]
                (let [perimeter (calculate-perimeter area)]
                  (* perimeter (count area)))))
         (reduce +))))

(solve1 sample-input)
(solve1 sample-input-large)
(solve1 (slurp "resources/day12/input.txt"))

;; part 2
(defn find-edges [area-coords]
  (let [coords (set area-coords)]
    (->> (for [coord coords
               edge-type [:top :bottom :left :right]
               :let [[x y] coord
                     neighbor (case edge-type
                                :top    [(dec x) y]
                                :bottom [(inc x) y]
                                :left   [x (dec y)]
                                :right  [x (inc y)])]
               :when (not (coords neighbor))]
           {:type edge-type :coord coord})
         set)))

(defn are-connected? [edge1 edge2]
  (let [{type1 :type [x1 y1] :coord} edge1
        {type2 :type [x2 y2] :coord} edge2]
    (and
     (= type1 type2)  ; must be the same type
     (case type1
       (:left :right)
       (and (= y1 y2)                    ; same y coordinate
            (= 1 (abs (- x1 x2))))       ; consecutive x coordinates
       (:top :bottom)
       (and (= x1 x2)                    ; same x coordinate
            (= 1 (abs (- y1 y2))))))))   ; consecutive y coordinates

(defn find-connected-group [edges edge]
  (loop [to-check #{edge}
         result #{}]
    (if (empty? to-check)
      result
      (let [current (first to-check)
            connected (set (filter #(and (not= % current)
                                         (are-connected? current %))
                                   edges))]
        (recur (into (disj to-check current) (remove result connected))
               (conj result current))))))

(defn count-sides [edges]
  (loop [remaining edges
         groups []]
    (if (empty? remaining)
      (count groups)
      (let [edge (first remaining)
            group (find-connected-group remaining edge)]
        (recur (apply disj remaining group)
               (conj groups group))))))

(find-edges #{[1 2] [2 2] [2 3] [3 3]})
(count-sides (find-edges #{[1 2] [2 2] [2 3] [3 3]}))

(are-connected? {:type :left :coord [1 2]} {:type :left :coord [1 3]})
(are-connected? {:type :left :coord [1 2]} {:type :bottom :coord [1 3]})

(def another-input "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")
(find-connected-areas another-input)

(defn solve2 [input]
  (let [areas (find-connected-areas input)]
    (->> areas
         (map (fn [area]
                (let [area-count (count area)]
                  (* area-count (count-sides (find-edges area))))))
         (reduce +))))

(solve2 another-input)
(solve2 (slurp "resources/day12/input.txt"))