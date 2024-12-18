(ns aoc2024.day18.solution
  (:require
   [clojure.string :as str]))

(def sample-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(str/blank? " ")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (mapv #(mapv (fn [s] (Integer/parseInt s)) %))))

(parse-input sample-input)

(defn get-neighbors [position max-x max-y]
  (let [[x y] position]
    (filter (fn [[nx ny]]
              (and (<= 0 nx max-x)
                   (<= 0 ny max-y)))
            [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])))

(defn initialize-state [from]
  {:queue (atom (conj clojure.lang.PersistentQueue/EMPTY from))
   :visited (atom #{from})
   :parent (atom {from nil})})

(defn reconstruct-path [parent to]
  (loop [path []
         node to]
    (if node
      (recur (conj path node) (@parent node))
      (reverse path))))

(defn process-neighbors [current state occupied-positions max-x max-y]
  (doseq [neighbor (get-neighbors current max-x max-y)]
    (when (and (not (contains? @(:visited state) neighbor))
               (not (contains? occupied-positions neighbor)))
      (swap! (:visited state) conj neighbor)
      (swap! (:parent state) assoc neighbor current)
      (swap! (:queue state) conj neighbor))))

(defn find-shortest-path [from to occupied-positions max-x max-y]
  (let [state (initialize-state from)]
    (loop []
      (when (seq @(:queue state))
        (let [current (peek @(:queue state))]
          (swap! (:queue state) pop)
          (if (= current to)
            (reconstruct-path (:parent state) to)
            (do
              (process-neighbors current state occupied-positions max-x max-y)
              (recur))))))))

(defn solve1 [input n max-x max-y]
  (let [parsed (parse-input input)
        blocked (set (take n parsed))]
    (->> (find-shortest-path [0 0] [max-x max-y] blocked max-x max-y)
         count
         dec)))

(comment
  (def occupied-positions #{[1 1] [2 2] [3 3]})

  (find-shortest-path [0 0] [4 4] occupied-positions 4 4)
  (find-shortest-path [0 0] [1 1] #{[0 1]} 1 1)

  (count (find-shortest-path [0 0] [6 6] (set (take 12 (parse-input sample-input))) 6 6))
  (solve1 sample-input 12 6 6)
  (solve1 (slurp "resources/day18/input.txt") 1024 70 70))

;; part 2

(defn path-blocked [parsed max-x max-y i]
  (nil? (find-shortest-path [0 0]
                            [max-x max-y]
                            (set (take i parsed))
                            max-x max-y)))

(defn solve2 [input max-x max-y]
  (let [parsed (parse-input input)
        max-attempts 10000]
    (->> (range max-attempts)
         (filter (partial path-blocked parsed max-x max-y))
         first
         dec
         (nth parsed))))

(solve2 sample-input 6 6)
(solve2 (slurp "resources/day18/input.txt") 70 70)
