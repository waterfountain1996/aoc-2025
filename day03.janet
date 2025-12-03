(import spork/test)
(use "./common")

(def test-input `987654321111111
811111111111119
234234234234278
818181911112111`)

(def input (string/trim (slurp "inputs/day03.txt")))

(defn parse-input
  [input]
  (def lines (string/split "\n" (string/trim input)))
  (def banks (map (fn [line] (map |(- $ (chr "0")) line)) lines))
  banks)

(defn max-joltage
  [bank]
  (max-of
    (seq [i :range [0 (length bank)]]
      (def combinations
        (seq [j :range [(inc i) (length bank)]]
          (+ (* (get bank i) 10) (get bank j))))
      (if (empty? combinations) 0 (max-of combinations)))))

(defn find-max-index
  "Returns the index of the largest number in arrtup"
  [arrtup]
  (var res 0)
  (loop [[i val] :pairs arrtup]
    (when (> val (get arrtup res)) (set res i)))
  res)

(defn max-joltage2
  [bank]
  (def [_ jolts]
    (reduce
      (fn
        [[bank jolts] power]
        (def idx (find-max-index (tuple/slice bank 0 (- (length bank) power))))
        [(tuple/slice bank (inc idx))
         (+ jolts (* (get bank idx) (math/pow 10 power)))])
      [bank 0]
      (range 11 -1 -1)))
  jolts)

(test/start-suite :gold)
(loop [[bank want] :in [[[9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] 987654321111]
                        [[8 1 1 1 1 1 1 1 1 1 1 1 1 1 9] 811111111119]
                        [[2 3 4 2 3 4 2 3 4 2 3 4 2 7 8] 434234234278]
                        [[8 1 8 1 8 1 9 1 1 1 1 2 1 1 1] 888911112111]]]
  (def got (max-joltage2 bank))
  (test/assert (= got want) (string/format "expected %d, got %d" want got)))
(test/end-suite)

(defn solve
  [part input]
  (def banks (parse-input input))
  (case part
    :p1 (apply + (map max-joltage banks))
    :p2 (apply + (map max-joltage2 banks))))

(print (solve :p1 input))
(print (solve :p2 input))
