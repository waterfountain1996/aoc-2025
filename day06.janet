(use "./common")

(def test-input `123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +`)

(def input (string/trim (slurp "inputs/day06.txt")))

(defn to-op
  [s]
  (case s
    "+" +
    "*" *
    error))

(defn parse-input1
  [input]
  (def grammar '{:cell (<- (+ :d+ (set "+*")))
                 :main (* :s* :cell (some (* :s+ :cell)))})
  (def rows (map |(peg/match grammar $) (string/split "\n" input)))
  (def nums (map (fn [row] (map scan-number row)) (array/slice rows 0 -2)))
  (def ops (map to-op (last rows)))
  [nums ops])

(defn solve-p1
  [nums ops]
  (sum (map (fn [col]
              (apply (get ops col) (map (fn [row] (get-in nums [row col])) (range 0 (length nums)))))
            (range 0 (length ops)))))

(defn solve-p2
  [input]
  (def rows (string/split "\n" input))
  (def head (array/slice rows 0 -2))
  (def tail (last rows))
  (def ops (map to-op (peg/match '(some (* :s* (<- (set "+*")))) tail)))

  (def end (max-of (map length head)))
  (def cols (array/concat (peg/find-all '(set "+*") tail) [end]))

  (def numrows @[])
  (eachp [i start] (array/slice cols 0 -2)
    (def end (dec (get cols (inc i))))

    (def numrow @[])
    (loop [col :down-to [end start]]
      (def numz @[])
      (loop [[j row] :pairs head]
        (def c (get row col))
        (when (not= c (chr " "))
          (def n (- c (chr "0")))
          (array/push numz n)))
      (def num (reduce2 (fn [acc n] (+ (* acc 10) n)) numz))
      (when num (array/push numrow num)))

    (array/push numrows (apply (get ops i) numrow)))

  (sum numrows))

(defn solve
  [part input]
  (case part
    :p1 (solve-p1 ;(parse-input1 input))
    :p2 (solve-p2 input)))

(print (solve :p1 input))
(print (solve :p2 input))
