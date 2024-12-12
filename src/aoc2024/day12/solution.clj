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
        
        :else (let [area-coords (loop [to-visit #{[x y]}
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
                                           (conj area-visited current)))))]
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
