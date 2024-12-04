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
