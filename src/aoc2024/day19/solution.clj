(ns aoc2024.day19.solution
  (:require [clojure.string :as str]))

(def sample-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse-input [input]
  (let [[towels targets] (str/split input #"\n\n")]
    {:towels (mapv str/trim (str/split towels #","))
     :targets (str/split-lines targets)}))

(defn count-towel-combinations [towels target]
  (let [n (count target)
        dp (long-array (inc n))]
    ; initialize dp array with 0 apart from the first element empty string
    (dotimes [i (inc n)]
      (aset dp i (long 0)))
    (aset dp 0 (long 1))
    ; start building dp array where dp[i] represents the number of ways to construct the first i characters of the target using towels
    (doseq [i (range n)]
      (when (pos? (aget dp i))
        (let [remaining (subs target i)
              current-count (aget dp i)]
          (doseq [towel towels
                  :let [len (count towel)]
                  :when (and
                         (<= (+ i len) n)
                         (.startsWith remaining towel))]
            (aset dp (+ i len)
                  (+ (aget dp (+ i len)) current-count))))))
    (aget dp n)))

(defn solve1 [input]
  (let [parsed (parse-input input)
        towels (:towels parsed)
        targets (:targets parsed)]
    (->> targets
         (map #(count-towel-combinations towels %))
         (filter pos?)
         (count))))

(defn solve2 [input]
  (let [parsed (parse-input input)
        towels (:towels parsed)
        targets (:targets parsed)]
    (->> targets
         (map #(count-towel-combinations towels %))
         (reduce +))))

(comment
  (solve1 sample-input)
  (solve1 (slurp "resources/day19/input.txt"))
  (solve2 sample-input)
  (solve2 (slurp "resources/day19/input.txt")))
