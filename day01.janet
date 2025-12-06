(import spork/test)
(use "./common")

(def- test-input `L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`)

(def- input (string/trim (slurp "inputs/day01.txt")))

(defn- reducer
  [acc m]
  (def [pos rotations] acc)
  (def end (% (+ pos m) 100))
  [(if (< end 0) (+ 100 end) end)
   (if (= end 0) (inc rotations) rotations)])

(defn- reducer2
  [acc turn]
  (def [pos rotations] acc)

  (def raw (+ pos turn))

  (def zeros (+
               (floordiv (math/abs raw) 100)
               (bool2num (and (not (zero? pos)) (<= raw 0)))))

  [(wrapmod raw 100)
   (+ rotations zeros)])

(defn- parse-input
  [input]
  (def grammar '{:main (* (<- (set "LR")) (<- :d+))})
  (def lines (map |(string/trim $) (string/split "\n" (string/trim input))))
  (def instructions (map |(peg/match grammar $) lines))
  (def moves
    (map
      (fn [instr]
        (def n (scan-number (get instr 1)))
        (if (= (get instr 0) "L") (- n) n))
      instructions))
  moves)

(defn solve
  [part input]
  (def moves (parse-input input))
  (case part
    :p1 (get (reduce reducer [50 0] moves) 1)
    :p2 (get (reduce reducer2 [50 0] moves) 1)))

(test/start-suite :silver)
(def- r1 (solve :p1 test-input))
(test/assert (= r1 3) (string/format "expected 3, got %d" r1))
(test/end-suite)

(test/start-suite :gold)
(def- r2 (solve :p2 test-input))
(test/assert (= r2 6) (string/format "expected 6, got %d" r2))
(test/end-suite)

(print (solve :p1 input))
(print (solve :p2 input))
