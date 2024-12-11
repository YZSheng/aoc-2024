(ns aoc2024.day11.solution
  (:require [clojure.string :as str]))

(def sample-input "0 1 10 99 999")

(defn parse-input [input]
  (->> (str/split input #" ")
       (map #(Integer/parseInt %))))

(parse-input sample-input)


(defn split-number [num]
  (let [s (str num)
        mid (/ (count s) 2)]
    [(Integer/parseInt (subs s 0 mid))
     (Integer/parseInt (subs s mid))]))

(split-number 1234)
(split-number 253000)

(defn blink [num]
  (cond
    (zero? num) [1]

    (zero? (mod (count (str num)) 2))
    (split-number num)

    :else [(* 2024 num)]))


(defn blink-flatten [num]
  (concat (blink num)))

(defn solve1 [input max-count]
  (let [parsed-input (parse-input input)]
    (loop [parsed-input parsed-input
           c 0
           result []]
      (if (= max-count c)
        (count result)
        (let [after-blink (->> parsed-input
                               (map blink-flatten)
                               (apply concat))]
          (recur after-blink
                 (inc c)
                 after-blink))))))



(solve1 "125 17" 25)
(solve1 "6 11 33023 4134 564 0 8922422 688775" 25)

(comment

  (parse-input sample-input)

  (->> (parse-input sample-input)
       (map blink-flatten)
       (apply concat))

  (str 12)
  (count "1234"))