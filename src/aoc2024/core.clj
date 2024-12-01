(ns aoc2024.core
  (:require [clojure.string :as str]))

(defn read-lines [input]
  (->> input
       slurp
       str/split-lines))

