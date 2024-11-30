(ns aoc2024.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-lines-of-numbers [input]
  (->> input
       slurp
       str/split-lines))
