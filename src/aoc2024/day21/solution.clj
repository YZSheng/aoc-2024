(ns aoc2024.day21.solution
  (:require
   [aoc2024.core :refer [find-all-shortest-paths]]
   [clojure.string :as str]))

(def sample-input "029A
980A
179A
456A
379A")

(def final-keypad [[\7 \8 \9] [\4 \5 \6] [\1 \2 \3] [\X \0 \A]])
(def intermediate-keypad [[\X \^ \A] [\< \v \>]])

(defn find-in-keypad [keypad value]
  (first (for [row (range (count keypad))
               col (range (count (keypad row)))
               :when (= (get-in keypad [row col]) value)]
           [row col])))

(defn translate-movements-to-chars [movements]
  (let [pairs (partition 2 1 movements)]
    (apply str (mapcat (fn [[[from-x from-y] [to-x to-y]]]
                         (cond
                           (= from-x to-x) (if (< from-y to-y) [\>] [\<])
                           (= from-y to-y) (if (< from-x to-x) [\v] [\^])
                           :else (throw (IllegalArgumentException. "Invalid movement"))))
                       pairs))))

(translate-movements-to-chars '([3 2] [3 1] [2 1]))

(defn construct-new-paths-for-final-keypad [paths next-position blocked-position]
  (for [[current-pos current-path] paths
        shortest-path (find-all-shortest-paths current-pos
                                               next-position
                                               #{blocked-position}
                                               4
                                               3)
        :let [movement (translate-movements-to-chars shortest-path)]]
    [next-position (str current-path movement "A")]))

(defn find-all-paths-for-final-keypad [inputs]
  (let [input-chars (map first (str/split inputs #""))
        start-position (find-in-keypad final-keypad \A)
        blocked-position [3 0]]
    (loop [paths #{[start-position ""]}
           input-chars input-chars]
      (if (seq input-chars)
        (let [next-char (first input-chars)
              next-position (find-in-keypad final-keypad next-char)
              new-paths (construct-new-paths-for-final-keypad paths next-position blocked-position)]
          (recur (into #{} new-paths)
                 (rest input-chars)))
        (map second paths)))))

(find-all-paths-for-final-keypad "029A")

(defn distance-to-a [pos]
  (let [a-pos [0 2]]
    (+ (Math/abs (- (first pos) (first a-pos)))
       (Math/abs (- (second pos) (second a-pos))))))


(defn construct-new-paths-for-intermediate-keypad [paths next-position blocked-position keep-best-n]
  (->> (for [[current-pos path-str] paths
             shortest-path (find-all-shortest-paths current-pos
                                                    next-position
                                                    #{blocked-position}
                                                    2
                                                    3)
             :let [movement (translate-movements-to-chars shortest-path)]]
         [next-position (str path-str movement "A")])
       (sort-by (fn [[pos _]] (distance-to-a pos)))
       (take keep-best-n)
       (into #{})))

(defn find-paths-for-intermediate-keypad [input]
  (let [input-chars (map first (str/split input #""))
        start-position (find-in-keypad intermediate-keypad \A)
        blocked-position [0 0]
        keep-best-n 20] ;; so random, trial and error
    (loop [paths #{[start-position ""]}
           remaining-chars input-chars]
      (if (seq remaining-chars)
        (let [next-char (first remaining-chars)
              next-position (find-in-keypad intermediate-keypad next-char)
              new-paths (construct-new-paths-for-intermediate-keypad paths next-position blocked-position keep-best-n)]
          (recur new-paths
                 (rest remaining-chars)))
        (map second paths)))))

(defn process-input [input]
  (let [final-paths (find-all-paths-for-final-keypad input)]
    (->> final-paths
         (mapcat find-paths-for-intermediate-keypad)
         (mapcat find-paths-for-intermediate-keypad)
         (sort-by count)
         first
         count)))

(process-input "029A")
(process-input "980A")
(process-input "179A")
(process-input "456A")
(process-input "379A")

(defn solve1 [inputs]
  (->> inputs
       str/split-lines
       (map (fn [input]
              (let [result (process-input input)
                    num (Integer/parseInt (str/replace input #"\D" ""))]
                (println input result num)
                (* num result))))
       (apply +)))

(Integer/parseInt (str/replace "179A" #"\D" ""))
(Integer/parseInt (str/replace "029A" #"\D" ""))

(solve1 sample-input)

(solve1 "208A
540A
685A
879A
826A")

;; part 2

(def bfs-directions
  {\^ [0 -1]
   \> [1 0]
   \v [0 1]
   \< [-1 0]})

(def keypad
  {\7 [0 0] \8 [1 0] \9 [2 0]
   \4 [0 1] \5 [1 1] \6 [2 1]
   \1 [0 2] \2 [1 2] \3 [2 2]
   \X [0 3] \0 [1 3] \A [2 3]})

(def directions
  {\X [0 0]
   \^ [1 0]
   \A [2 0]
   \< [0 1]
   \v [1 1]
   \> [2 1]})

(defn valid-position? [input pos]
  (and (>= (first pos) 0)
       (>= (second pos) 0)
       (<= (first pos) 2)
       (<= (second pos) 3)
       (not= pos (get input \X))
       (some #(= pos (second %)) input)))

(defn valid-path? [input start-pos path]
  (loop [pos start-pos
         moves (butlast (seq path))]  ; Remove the A
    (if (empty? moves)
      true
      (let [dir (first moves)
            [dx dy] (get bfs-directions dir)
            new-pos [(+ (first pos) dx) (+ (second pos) dy)]]
        (if (valid-position? input new-pos)
          (recur new-pos (rest moves))
          false)))))

(defn get-command [input start end]
  (if (= start end)
    ["A"]
    (let [start-pos (get input start)
          end-pos (get input end)
          dx (- (first end-pos) (first start-pos))
          dy (- (second end-pos) (second start-pos))
          ;; Generate direct paths based on dx and dy
          horizontal-first (str
                            (apply str (repeat (abs dx) (if (pos? dx) \> \<)))
                            (apply str (repeat (abs dy) (if (pos? dy) \v \^)))
                            "A")
          vertical-first (str
                          (apply str (repeat (abs dy) (if (pos? dy) \v \^)))
                          (apply str (repeat (abs dx) (if (pos? dx) \> \<)))
                          "A")
          paths (cond-> []
                  (valid-path? input start-pos horizontal-first) (conj horizontal-first)
                  (valid-path? input start-pos vertical-first) (conj vertical-first))]
      (if (seq paths)
        (sort-by count paths)
        ["A"]))))

(def get-key-presses
  (memoize
   (fn [input code robot]
     (let [result
           (loop [current \A
                  chars (seq code)
                  length 0]
             (if (empty? chars)
               length
               (let [next-char (first chars)
                     moves (get-command input current next-char)
                     move-length (if (zero? robot)
                                   (count (first moves))
                                   (let [sub-lengths (keep #(get-key-presses directions % (dec robot)) moves)]
                                     (if (seq sub-lengths)
                                       (apply min sub-lengths)
                                       1)))]
                 (recur next-char
                        (rest chars)
                        (+ length move-length)))))]
       result))))

(defn min-commands-to-reach [n target-sequence]
  (get-key-presses keypad target-sequence n))

(min-commands-to-reach 2 "029A")
(min-commands-to-reach 2 "980A")
(min-commands-to-reach 2 "179A")
(min-commands-to-reach 2 "456A")
(min-commands-to-reach 2 "379A")

(defn solve2 [inputs]
  (->> inputs
       str/split-lines
       (map (fn [input]
              (let [result (min-commands-to-reach 25 input)
                    num (Integer/parseInt (str/replace input #"\D" ""))]
                (* num result))))
       (apply +)))

(solve2 "208A
540A
685A
879A
826A")