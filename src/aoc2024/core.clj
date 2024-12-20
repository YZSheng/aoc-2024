(ns aoc2024.core
  (:require [clojure.string :as str]))

(defn read-lines [input]
  (->> input
       slurp
       str/split-lines))

(defn read-single-line [input]
  (slurp input))

(defn read-char-map [input]
  (->> input
       str/split-lines
       (mapv #(vec (str/split % #"")))))

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