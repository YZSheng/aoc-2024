(ns aoc2024.day10.solution
   (:require
    [clojure.string :as str]))

 (def sample-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

 (defn parse-input [input]
   (->> input
        (str/split-lines)
        (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #""))))))

 (parse-input sample-input)

 (defn find-zeros [input]
   (let [parsed-input (parse-input input)]
     (for [row (range (count parsed-input))
           col (range (count (first parsed-input)))
           :when (zero? (get-in parsed-input [row col]))]
       [row col])))

 (find-zeros sample-input)

 (defn find-all-neighbors [input row col]
   (let [parsed-input (parse-input input)
         up-coord [(dec row) col]
         down-coord [(inc row) col]
         left-coord [row (dec col)]
         right-coord [row (inc col)]
         up (get-in parsed-input up-coord)
         down (get-in parsed-input down-coord)
         left (get-in parsed-input left-coord)
         right (get-in parsed-input right-coord)]
     (->> [{:coord up-coord
            :value up}
           {:coord down-coord
            :value down}
           {:coord left-coord
            :value left}
           {:coord right-coord
            :value right}]
          (filter (fn [{:keys [value]}] (some? value))))))

 (find-all-neighbors sample-input 0 0)

 (get-in (parse-input sample-input) [-1 0])

(defn calculate-score [input row col]
  (let [parsed-input (parse-input input)]
    (loop [to-visit #{[row col]}
           graph {}]
      (if (empty? to-visit)
        graph
        (let [current (first to-visit)
              current-value (get-in parsed-input current)
              neighbors (find-all-neighbors input (first current) (second current))
              next-neighbors (filter #(= (inc current-value) (:value %)) neighbors)
              next-coords (map :coord next-neighbors)]
          (recur 
            (into (disj to-visit current) 
                  (filter #(not (contains? graph %)) next-coords))
            (assoc graph current 
                   {:coord current
                    :value current-value
                    :next-coords next-coords})))))))

(defn count-paths [graph]
  (->> graph
       vals
       (filter #(= 9 (:value %)))
       count))
 
(calculate-score sample-input 0 2)
(count-paths (calculate-score sample-input 0 2))

(defn solve1 [input]
  (let [zeros (find-zeros input)]
    (->> zeros
         (map (fn [[row col]] (count-paths (calculate-score input row col))))
         (reduce +))))

(solve1 sample-input)
(solve1 (slurp "resources/day10/input.txt"))
