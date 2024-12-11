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

(defn frequency-updater [freq cached-freqs]
  (fn [current-freqs]
    (reduce-kv
     (fn [m k v]
       (update m k (fnil + 0) (* freq v)))
     current-freqs
     cached-freqs)))

(defn get-next-state [seen-states num-freqs count-result]
  (reduce-kv
   (fn [acc n freq]
     (if-let [state (get seen-states n)]
       (let [cached-freqs (:nums-after-cycle state)]
         (-> acc
             (update :freqs
                     (frequency-updater freq cached-freqs))
             (update :count + (* freq (:count-increment state)))))
       (let [blinked (blink n)
             count-increment (dec (count blinked))
             blink-freqs (frequencies blinked)]
         (-> acc
             (update :freqs
                     (frequency-updater freq blink-freqs))
             (update :count + (* freq count-increment))
             (update :seen assoc n
                     {:nums-after-cycle blink-freqs
                      :count-increment count-increment})))))
   {:freqs {} :seen seen-states :count count-result}
   num-freqs))


(defn solve1 [input max-count]
  (let [parsed-input (parse-input input)]
    (loop [num-freqs (frequencies parsed-input)
           c 0
           count-result (count parsed-input)
           seen-states {}]
      (if (= max-count c)
        count-result
        (let [next-state (get-next-state seen-states num-freqs count-result)]
          (recur (:freqs next-state)
                 (inc c)
                 (:count next-state)
                 (:seen next-state)))))))

(solve1 "125 17" 25)
(solve1 "125 17" 35)
(solve1 "125 17" 40)
(solve1 "6 11 33023 4134 564 0 8922422 688775" 75)

(comment

  (parse-input sample-input)

  (->> (parse-input sample-input)
       (map blink-flatten)
       (apply concat))

  (str 12)
  (count "1234"))