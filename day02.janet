(use "./common")

(def test-input `11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124`)

(def input (string/trim (slurp "inputs/day02.txt")))

(defn parse-input
  [input]
  (def sranges (string/split "," (string/trim input)))
  (def tranges (map |(string/split "-" $) sranges))
  (def ranges (map (fn [tup]
                     [(scan-number (get tup 0))
                      (scan-number (get tup 1))]) tranges))
  ranges)

(defn invalid-id?
  [id]
  (def sid (string/format "%d" id))
  (def len (length sid))
  (def half (floordiv len 2))
  (if (zero? (% len 2))
    (= (string/slice sid 0 half) (string/slice sid half len))
    false))

(defn invalid-id2?
  [id]
  (def sid (string/format "%d" id))
  (def len (length sid))
  (def half (floordiv len 2))

  (def seqs (map |(string/slice sid 0 $) (range 1 (inc len))))
  (any? (map
          (fn
            [s]
            (def rpt (floordiv len (length s)))
            (and (= sid (string/repeat s rpt)) (>= rpt 2)))
          seqs)))

(defn make-reducer
  [checker]
  (fn
    [acc r]
    (+
      acc
      (apply + (filter checker
                       (range (get r 0) (inc (get r 1))))))))

(defn solve
  [part input]
  (def ranges (parse-input input))
  (case part
    :p1 (reduce (make-reducer invalid-id?) 0 ranges)
    :p2 (reduce (make-reducer invalid-id2?) 0 ranges)))

(pp (solve :p1 input))
(pp (solve :p2 input))
