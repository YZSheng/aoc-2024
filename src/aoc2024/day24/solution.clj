(ns aoc2024.day24.solution
  (:require
   [clojure.string :as str]))

(def sample-input-simple "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
")

(defn and-op [a b]
  (if (= 1 a b) 1 0))

(and-op 1 1)
(and-op 0 1)
(and-op 1 0)
(and-op 0 0)

(defn or-op [a b]
  (if (or (= 1 a) (= 1 b)) 1 0))

(or-op 1 0)
(or-op 0 0)
(or-op 1 1)
(or-op 0 1)

(defn xor-op [a b]
  (if (not= a b) 1 0))

(xor-op 1 0)
(xor-op 1 1)
(xor-op 0 1)
(xor-op 0 0)

(defn parse-input [input]
  (->> input
       (#(str/split % #"\n\n"))
       ((fn [[wires instructions]]
          [(into {} (map (fn [wire]
                           (let [[wire-name wire-value] (str/split wire #": ")]
                             [wire-name (parse-long wire-value)])) (str/split-lines wires)))
           (mapv (fn [instruction]
                   (let [[inputs output] (str/split instruction #" -> ")
                         [input1 op input2] (str/split inputs #" ")]
                     [op input1 input2 output])) (str/split-lines instructions))]))))

(parse-input sample-input-simple)

(defn get-op [op-name]
  (case op-name
    "AND" and-op
    "OR" or-op
    "XOR" xor-op))

(defn run [wires instructions]
  (loop [current-wires wires]
    (let [new-wires (reduce (fn [wires [op a b output]]
                              (if (and (contains? wires a)
                                       (contains? wires b))
                                (assoc wires output ((get-op op) (wires a) (wires b)))
                                wires))
                            current-wires
                            instructions)]
      (if (= new-wires current-wires)
        new-wires
        (recur new-wires)))))

(defn solve1 [input]
  (let [[wires instructions] (parse-input input)]
    (->> (run wires instructions)
         (filter (fn [[k _]] (str/starts-with? k "z")))
         (sort-by first)
         (map second)
         reverse
         (apply str)
         (#(Long/parseLong % 2)))))

(solve1 sample-input-simple)

(def sample-input-large "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
")

(solve1 sample-input-large)
(solve1 (slurp "resources/day24/input.txt"))

;; part 2
;; https://www.gsnetwork.com/wp-content/uploads/2023/01/full-adder-xor-gate-circuit-diagram.jpg
(defn detect-swap [instructions i]
  (let [xn (format "x%02d" i)
        yn (format "y%02d" i)
        zn (format "z%02d" i)
        and-gate (first (filter #(and (= (first %) "AND")
                                      (#{xn yn} (second %))
                                      (#{xn yn} (nth % 2)))
                                instructions))
        xor-gate (first (filter #(and (= (first %) "XOR")
                                      (#{xn yn} (second %))
                                      (#{xn yn} (nth % 2)))
                                instructions))
        sum-gate (first (filter #(and (= (first %) "XOR")
                                      (or (= (second %) (last xor-gate))
                                          (= (nth % 2) (last xor-gate))))
                                instructions))
        and-xor-carry (first (filter #(and (= (first %) "AND")
                                           (or (= (second %) (last xor-gate))
                                               (= (nth % 2) (last xor-gate))))
                                     instructions))
        carry-out (when (and and-gate and-xor-carry)
                    (first (filter #(and (= (first %) "OR")
                                         (= #{(second %) (nth % 2)}
                                            #{(last and-gate) (last and-xor-carry)}))
                                   instructions)))]
    (cond
      ;; sum-gate is not output to the correct z-bit
      (and sum-gate (not= (last sum-gate) zn))
      [(last sum-gate) zn]

      ;; no sum-gate or carry-out or and-xor-carry
      (not (or sum-gate carry-out and-xor-carry))
      [(last and-gate) (last xor-gate)])))

(defn solve2 [input]
  (let [[_ instructions] (parse-input input)
        n (count (filter #(str/starts-with? (last %) "z") instructions))
        swaps (->> (range 1 (dec n))
                   (keep #(detect-swap instructions %)))]
    (->> swaps
         (apply concat)
         sort
         (str/join ","))))

(solve2 (slurp "resources/day24/input.txt"))
