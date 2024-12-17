(ns aoc2024.day17.solution
  (:require [clojure.string :as str]))

;; Combo operands 0 through 3 represent literal values 0 through 3.
;; Combo operand 4 represents the value of register A.
;; Combo operand 5 represents the value of register B.
;; Combo operand 6 represents the value of register C.
;; Combo operand 7 is reserved and will not appear in valid programs.

(def sample-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(def op-map {0 0
             1 1
             2 2
             3 3
             4 :A
             5 :B
             6 :C
             7 nil})

(op-map 6)
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

(adv {:A 2024} 1)

(defn bxl [m arg]
  (let [register-b (get m :B)
        bitwise-xor (bit-xor register-b arg)]
    (assoc m :B bitwise-xor)))

(defn bst [m value]
  (let [combo-value (get-combo-value m value)
        module-8 (mod combo-value 8)]
    (assoc m :B module-8)))

; {:C 9, :B 1}
(bst {:C 9} 6)

(defn update-instructions [instructions value]
  (subvec instructions value))

(update-instructions [1 2 3 4 5 6 7 8 9 10] 2)

(defn jnz [m value latest-instructions original-instructions]
  (let [a-register (get m :A)]
    (if (zero? a-register)
      [m (drop 2 latest-instructions)]
      [m (update-instructions original-instructions value)])))

(defn bxc [m value]
  (let [b-register (get m :B)
        c-register (get m :C)
        bitwise-xor (bit-xor b-register c-register)]
    (assoc m :B bitwise-xor)))

(defn out [m value]
  (let [combo-value (get-combo-value m value)
        module-8 (mod combo-value 8)]
    (do
      (println "Output::" module-8)
      m)))

(out {:A 10} 0)


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

(defn process-single-instruction [m [op-code arg] latest-instructions original-instructions]
  (case op-code
    0 [(adv m arg) (drop 2 latest-instructions)]
    1 [(bxl m arg) (drop 2 latest-instructions)]
    2 [(bst m arg) (drop 2 latest-instructions)]
    3 (jnz m arg latest-instructions original-instructions)
    4 [(bxc m arg) (drop 2 latest-instructions)]
    5 [(out m arg) (drop 2 latest-instructions)]
    6 [(bdv m arg) (drop 2 latest-instructions)]
    7 [(cdv m arg) (drop 2 latest-instructions)]))

(defn process-instructions [m original-instructions]
  (loop [m m
         instructions original-instructions]
    (if (empty? instructions)
      m
      (let [[new-m new-instructions]
            (process-single-instruction m [(first instructions) (second instructions)] instructions original-instructions)]
        (recur new-m new-instructions)))))

(process-instructions {:C 9} [2 6])
(process-instructions {:A 10} [5 0])
(process-instructions {:A 10} [5 1])
(process-instructions {:A 10} [5 2])
(process-instructions {:A 10} [5 0 5 1 5 2])

(process-instructions {:A 2024} [0 1 5 4 3 0])

(process-instructions {:B 29} [1 7])

(process-instructions {:B 2024 :C 43690} [4 0])

(process-instructions {:A 729 :B 0 :C 0} [0 1 5 4 3 0])
(process-instructions {:A 64854237 :B 0 :C 0} [2 4 1 1 7 5 1 5 4 0 5 5 0 3 3 0])