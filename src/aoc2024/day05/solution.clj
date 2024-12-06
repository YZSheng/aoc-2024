(ns aoc2024.day05.solution
  (:require [clojure.string :as str]))

(def sample-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

sample-input

(defn parse-input [input]
  (let [[rules pages] (str/split input #"\n\n")]
    [(->> (str/split-lines rules)
          (map #(str/split % #"\|"))
          (map (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)])))
     (->> (str/split-lines pages)
          (map #(str/split % #","))
          (map (fn [line] (map #(Integer/parseInt %) line))))]))

(def sample-rules '([47 53]
                    [97 13]
                    [97 61]
                    [97 47]
                    [75 29]
                    [61 13]
                    [75 53]
                    [29 13]
                    [97 29]
                    [53 29]
                    [61 53]
                    [97 53]
                    [61 29]
                    [47 13]
                    [75 47]
                    [97 75]
                    [47 61]
                    [75 61]
                    [47 29]
                    [75 13]
                    [53 13]))

(defn check-page-by-rules [page rules]
  (let [page-pairs (partition 2 1 page)]
    (every? (fn [page-pair] (some #(= page-pair %) rules)) page-pairs)))

(defn middle-value [nums]
  (let [n (count nums)
        mid (quot n 2)]
    (if (odd? n)
      (nth nums mid)
      (/ (+ (nth nums mid)
            (nth nums (dec mid)))
         2.0))))

(defn solve1 [input]
  (let [[rules pages] (parse-input input)]
    (->> pages
         (filter (fn [page] (check-page-by-rules page rules)))
         (map middle-value)
         (reduce +))))

(solve1 sample-input)
(solve1 (slurp "resources/day05/input.txt"))

;; part 2

(defn build-graph [pages rules]
  (let [pages-set (set pages)
        relevant-rules (filter (fn [[a b]]
                                 (and (contains? pages-set a)
                                      (contains? pages-set b)))
                               rules)]
    (reduce (fn [acc [from to]]
              (-> acc
                  (update-in [from :out] (fnil conj #{}) to)
                  (update-in [to :in] (fnil conj #{}) from)))
            {}
            relevant-rules)))

(defn compare-pages [graph a b]
  (let [a-score (- (count (get-in graph [a :out] #{}))
                   (count (get-in graph [a :in] #{})))
        b-score (- (count (get-in graph [b :out] #{}))
                   (count (get-in graph [b :in] #{})))]
    (compare b-score a-score)))

(defn sort-by-rules [pages rules]
  (let [graph (build-graph pages rules)]
    (sort (partial compare-pages graph) pages)))

(comment
  (def test-pages [61 13 29])
  (def test-rules [[61 13] [29 13] [61 29]])

  (sort-by-rules test-pages test-rules))

(defn solve2 [input]
  (let [[rules pages] (parse-input input)]
    (->> pages
         (filter (fn [page] (not (check-page-by-rules page rules))))
         (map (fn [page] (sort-by-rules page rules)))
         (map middle-value)
         (reduce +))))

(solve2 sample-input)
(solve2 (slurp "resources/day05/input.txt"))