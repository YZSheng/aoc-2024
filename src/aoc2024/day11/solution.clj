(ns aoc2024.day11.solution
  (:require [clojure.string :as str]))

(def sample-input "0 1 10 99 999")

(defn parse-input [input]
  (->> (str/split input #" ")
       (map #(Integer/parseInt %))))

(defn split-number [num]
  (let [s (str num)
        mid (/ (count s) 2)]
    [(Integer/parseInt (subs s 0 mid))
     (Integer/parseInt (subs s mid))]))

(defn blink [num]
  (cond
    (zero? num) [1]

    (zero? (mod (count (str num)) 2))
    (split-number num)

    :else [(* 2024 num)]))

(defn get-next-freqs [num-freqs]
  (reduce-kv
   (fn [acc n freq]
     (let [blinked (blink n)]
       (reduce-kv
        (fn [m k v]
          (update m k (fnil + 0) (* freq v)))
        acc
        (frequencies blinked))))
   {}
   num-freqs))

(defn solve1 [input max-count]
  (let [parsed-input (parse-input input)]
    (loop [num-freqs (frequencies parsed-input)
           c 0]
      (if (= max-count c)
        (reduce + (vals num-freqs))
        (let [next-freqs (get-next-freqs num-freqs)]
          (recur next-freqs (inc c)))))))

(solve1 "125 17" 25)
(solve1 "125 17" 35)
(solve1 "125 17" 40)
(solve1 "6 11 33023 4134 564 0 8922422 688775" 75)

(comment

  (parse-input sample-input)

  (split-number 1234)
  (split-number 253000)

  (str 12)
  (count "1234"))