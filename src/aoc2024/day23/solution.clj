(ns aoc2024.day23.solution
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def sample-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (map set)))

(parse-input sample-input)

(defn build-graph [parsed]
  (reduce (fn [acc edge-set]
            (let [[a b] (seq edge-set)] 
              (-> acc
                  (update a (fnil conj #{}) b)
                  (update b (fnil conj #{}) a))))
          {}
          parsed))

(build-graph (parse-input sample-input))

(defn find-three-connected [input]
  (let [parsed (parse-input input)
        graph (build-graph parsed)]
    (->> graph
         (mapcat (fn [[node neighbors]]
           (for [n1 neighbors
                 n2 neighbors
                 :when (and (not= n1 n2)
                           (contains? (get graph n1) n2))]
             (hash-set node n1 n2))))
         distinct)))

(find-three-connected sample-input)
(count (find-three-connected sample-input))

(defn solve1 [input]
  (let [three-interconnected (find-three-connected input)]
    (->> three-interconnected
         (filter (fn [set] (some (fn [item] (.startsWith item "t")) set)))
         count)))

(solve1 sample-input)

(solve1 (slurp "resources/day23/input.txt"))