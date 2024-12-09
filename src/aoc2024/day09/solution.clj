(ns aoc2024.day09.solution
  (:require
   [clojure.string :as str]))

(def sample-input "2333133121414131402")

(defn parse-input [input]
  (let [split-input (str/split input #"")]
    (map-indexed (fn [idx elem] [idx elem]) (map #(Integer/parseInt %) split-input))))

(parse-input sample-input)

(defn translate-input [input]
  (let [parsed-input (parse-input input)]
    (->> parsed-input
         (map (fn [[idx elem]]
                (if (zero? (mod idx 2))
                  [(/ idx 2) (repeat elem (/ idx 2))]
                  [:space (repeat elem nil)])))
         (map second)
         (apply concat))))

(translate-input sample-input)

(defn find-nil-positions [coll]
  (reduce-kv (fn [acc idx val]
               (if (nil? val)
                 (conj acc idx)
                 acc))
             [] coll))

(find-nil-positions [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9])

(defn swap-nil-with-val [result nil-idx current-val current-idx]
  (-> result
      (assoc nil-idx current-val)
      (assoc current-idx nil)))

(defn swap-with-first-nil [coll]
  (let [v (vec coll)
        nil-positions (find-nil-positions v)]
    (if (empty? nil-positions)
      v
      (loop [result v
             nil-idx-pointer 0
             current-idx (dec (count v))]
        (if (or (< current-idx 0)
                (>= nil-idx-pointer (count nil-positions)))
          result
          (let [current-val (get result current-idx)
                nil-idx (nth nil-positions nil-idx-pointer)]
            (if (and current-val (< nil-idx current-idx))
              (recur (swap-nil-with-val result nil-idx current-val current-idx)
                     (inc nil-idx-pointer)
                     (dec current-idx))
              (recur result
                     nil-idx-pointer
                     (dec current-idx)))))))))

(swap-with-first-nil [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9])

(defn calculate-check-sum [l]
  (->> l
       (filter identity)
       (map-indexed (fn [idx elem] (* elem idx)))
       (reduce +)))

(calculate-check-sum (swap-with-first-nil [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9]))

(defn solve1 [input]
  (->> input
       (translate-input)
       (swap-with-first-nil)
       (calculate-check-sum)))

(solve1 sample-input)
(solve1 (slurp "resources/day09/input.txt"))

(->> sample-input
     translate-input)

;; part 2


(defn get-block-size [v idx]
  (let [val (get v idx)]  ;; Define val from the vector at idx
    (loop [size 1
           i (dec idx)]
      (if (and (>= i 0) (= (get v i) val))
        (recur (inc size) (dec i))
        size))))

;;returns [end-index size value]
(defn find-blocks [v]
  (loop [idx (dec (count v))
         blocks []]
    (if (< idx 0)
      blocks
      (let [val (get v idx)]
        (if (nil? val)
          (recur (dec idx) blocks)
          (let [size (get-block-size v idx)]
            (recur (- idx size) (conj blocks [idx size val]))))))))

(defn swap-with-first-nil-in-block [coll]
  (let [v (vec coll)
        ;; Find blocks and their sizes
        blocks (find-blocks v)]
    (println blocks)
    (loop [result v
           remaining-blocks blocks]
      (if (empty? remaining-blocks)
        result
        (let [[block-end block-size block-val] (first remaining-blocks)
              block-start (- block-end (dec block-size))
              ;; Find first position with enough consecutive nils
              first-fit (loop [pos 0]
                          (when (< pos block-start)
                            (if (and (nil? (get result pos))
                                     (every? nil? (take block-size (subvec result pos))))
                              pos
                              (recur (inc pos)))))]
          (if first-fit
            (recur (-> result
                      ;; Clear original position
                       (as-> r (reduce #(assoc %1 %2 nil) r (range block-start (inc block-end))))
                      ;; Place at new position
                       (as-> r (reduce #(assoc %1 %2 block-val) r (range first-fit (+ first-fit block-size)))))
                   (rest remaining-blocks))
            (recur result (rest remaining-blocks))))))))

(swap-with-first-nil-in-block [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9])


(defn solve2 [input]
  (->> input
       (translate-input)
       (swap-with-first-nil-in-block)
       (map (fn [x] (if (nil? x) 0 x)))
       (calculate-check-sum)))

(solve2 sample-input)
(solve2 (slurp "resources/day09/input.txt"))