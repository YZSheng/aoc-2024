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

(defn contains-substring? [s substr]
  (str/includes? s substr))

(defn remove-single-substring [target towel]
  (str/replace-first target towel ""))

(defn can-make-target? [towels target]
  (if (empty? target)
    true
    (some (fn [towel]
            (when (.startsWith target towel)
              (can-make-target?
               towels
               (subs target (count towel)))))
          towels)))

(comment
  (parse-input sample-input)
  (contains-substring? "brwrr" "br1")
  (remove-single-substring "brwrr" "br")
  (remove-single-substring "brbrwrr" "br")
  (remove-single-substring "brbrwrr" "br123")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "brwrr")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "ubwu")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "bggr")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "rrbgbr")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "bwurrg")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "bbrgwb")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "brgr")
  (can-make-target? ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"] "bwurrg"))

(defn solve1 [input]
  (let [parsed (parse-input input)
        towels (:towels parsed)
        targets (:targets parsed)]
    (count (filter #(can-make-target? towels %) targets))))

(solve1 sample-input)
(solve1 (slurp "resources/day19/input.txt"))

(def count-towel-combinations
  (memoize
   (fn [towels target]
     (if (empty? target)
       1
       (->> towels
            (keep (fn [towel]
                    (when (.startsWith target towel)
                      (let [remaining (subs target (count towel))]
                        (count-towel-combinations towels remaining)))))
            (reduce + 0))))))

(defn solve2 [input]
  (let [parsed (parse-input input)
        towels (:towels parsed)
        targets (:targets parsed)]
    (->> targets
         (map #(count-towel-combinations towels %))
         (reduce +))))

(solve2 sample-input)
(solve2 (slurp "resources/day19/input.txt"))
