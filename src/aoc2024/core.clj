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

(defn collect-all-paths [paths-atom parent-paths current]
  (swap! paths-atom update current
         (fnil into #{})
         (map #(conj % current) parent-paths)))

(defn find-all-shortest-paths [from to occupied-positions max-x max-y]
  (let [state {:queue (atom (conj clojure.lang.PersistentQueue/EMPTY from))
               :visited (atom #{from})
               :parent (atom {})
               :paths (atom {from #{[from]}})
               :min-depth (atom Integer/MAX_VALUE)}]
    (loop [depth 0]
      (if (seq @(:queue state))
        (let [level-size (count @(:queue state))
              current-depth depth]
          (doseq [_ (range level-size)]
            (let [current (peek @(:queue state))]
              (swap! (:queue state) pop)
              (if (= current to)
                (reset! (:min-depth state) current-depth)
                (when (<= current-depth @(:min-depth state))
                  (doseq [neighbor (get-neighbors current max-x max-y)]
                    (when (and (not (contains? occupied-positions neighbor))
                               (<= (inc current-depth) @(:min-depth state)))
                      (when-not (contains? @(:visited state) neighbor)
                        (swap! (:queue state) conj neighbor)
                        (swap! (:visited state) conj neighbor))
                      (collect-all-paths (:paths state)
                                         (@(:paths state) current)
                                         neighbor)))))))
          (recur (inc depth)))
        ;; Return all paths that reach the target with minimum length
        (filter #(= (count %) (inc @(:min-depth state)))
                (@(:paths state) to))))))

(defn transpose [matrix]
  (apply mapv vector matrix))
