(ns aoc2024.day17.solution 
  (:require
   [clojure.string :as str]))

;; Combo operands 0 through 3 represent literal values 0 through 3.
;; Combo operand 4 represents the value of register A.
;; Combo operand 5 represents the value of register B.
;; Combo operand 6 represents the value of register C.
;; Combo operand 7 is reserved and will not appear in valid programs.

(def op-map {0 0
             1 1
             2 2
             3 3
             4 :A
             5 :B
             6 :C
             7 nil})

(defn get-combo-value [m value]
  (let [op-map-value (op-map value)]
    (case op-map-value
      nil nil
      0 0
      1 1
      2 2
      3 3
      (get m op-map-value))))

(defn adv [m arg]
  (let [combo-value (get-combo-value m arg)
        numerator (get m :A)
        denominator (Math/pow 2 combo-value)
        result (long (quot numerator denominator))]
    (assoc m :A result)))

(defn bxl [m arg]
  (let [register-b (get m :B)
        bitwise-xor (bit-xor register-b arg)]
    (assoc m :B bitwise-xor)))

(defn bst [m value]
  (let [combo-value (get-combo-value m value)
        module-8 (mod combo-value 8)]
    (assoc m :B module-8)))

(defn update-instructions [instructions value]
  (subvec instructions value))

(defn jnz [m value latest-instructions original-instructions result]
  (let [a-register (get m :A)]
    (if (zero? a-register)
      [m (drop 2 latest-instructions) result]
      [m (update-instructions original-instructions value) result])))

(defn bxc [m value]
  (let [b-register (get m :B)
        c-register (get m :C)
        bitwise-xor (bit-xor b-register c-register)]
    (assoc m :B bitwise-xor)))

(defn out [m value result]
  (let [combo-value (get-combo-value m value)
        module-8 (mod combo-value 8)]
    [m (conj result module-8)]))


(defn bdv [m arg]
  (let [combo-value (get-combo-value m arg)
        numerator (get m :A)
        denominator (Math/pow 2 combo-value)
        result (long (quot numerator denominator))]
    (assoc m :B result)))

(defn cdv [m arg]
  (let [combo-value (get-combo-value m arg)
        numerator (get m :A)
        denominator (Math/pow 2 combo-value)
        result (long (quot numerator denominator))]
    (assoc m :C result)))

(defn process-single-instruction [m [op-code arg] latest-instructions original-instructions result]
  (case op-code
    0 [(adv m arg) (drop 2 latest-instructions) result]
    1 [(bxl m arg) (drop 2 latest-instructions) result]
    2 [(bst m arg) (drop 2 latest-instructions) result]
    3 (jnz m arg latest-instructions original-instructions result)
    4 [(bxc m arg) (drop 2 latest-instructions) result]
    5 (let [[updated-m updated-result] (out m arg result)]
        [updated-m (drop 2 latest-instructions) updated-result])
    6 [(bdv m arg) (drop 2 latest-instructions) result]
    7 [(cdv m arg) (drop 2 latest-instructions) result]))

(defn process-instructions [m original-instructions]
  (loop [m m
         instructions original-instructions
         result []]
    (if (empty? instructions)
      [m result]
      (let [[new-m new-instructions new-result]
            (process-single-instruction m [(first instructions) (second instructions)] instructions original-instructions result)]
        (recur new-m new-instructions new-result)))))

(comment
  (process-instructions {:C 9} [2 6])
  (process-instructions {:A 10} [5 0])
  (process-instructions {:A 10} [5 1])
  (process-instructions {:A 10} [5 2])
  (process-instructions {:A 10} [5 0 5 1 5 2])

  (process-instructions {:A 2024} [0 1 5 4 3 0])

  (process-instructions {:B 29} [1 7])

  (process-instructions {:B 2024 :C 43690} [4 0])

  (process-instructions {:A 729 :B 0 :C 0} [0 1 5 4 3 0])
  (process-instructions {:A 64854237 :B 0 :C 0} [2 4 1 1 7 5 1 5 4 0 5 5 0 3 3 0]))

(defn solve1 [a-value program]
  (let [result (process-instructions {:A a-value :B 0 :C 0} program)]
    (str/join "," (last result))))

(solve1 729 [0 1 5 4 3 0])
(solve1 64854237 [2 4 1 1 7 5 1 5 4 0 5 5 0 3 3 0])

;; part 2
(process-instructions {:A 117440 :B 0 :C 0} [0 3 5 4 3 0])

(defn solve2 [result]
  (let [reversed-result (reverse result)]
    (loop [power (- (count result) 1)
           open  #{(- (bit-shift-left 1 (* 3 (count result))) 1)}]
      (if (< power 0)
        (apply min open)
        (recur (dec power)
               (mapcat (fn [o]
                         (for [i (take 8 (iterate #(- % (bit-shift-left 1 (* 3 power))) o))
                               :let  [output (->> result
                                                  (process-instructions {:A i :B 0 :C 0})
                                                  last
                                                  reverse)]
                               :when (= (take (- (count result) power) output)
                                        (take (- (count result) power) reversed-result))]
                           i)) open))))))

(solve2 [0 3 5 4 3 0])
(solve2 [2 4 1 1 7 5 1 5 4 0 5 5 0 3 3 0])