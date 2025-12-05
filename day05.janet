(use "./common")

(def test-input `3-5
10-14
16-20
12-18

1
5
8
11
17
32`)

(def input (string/trim (slurp "inputs/day05.txt")))

(defn strs2nums
  [arrtup]
  (map scan-number arrtup))

(defn parse-input
  [input]
  (def [head tail] (string/split "\n\n" input))
  (def grammar '{:main (* (<- :d+) "-" (<- :d+))})
  (def ranges (map |(strs2nums (peg/match grammar $)) (string/split "\n" head)))
  (def ids (strs2nums (string/split "\n" tail)))
  [ranges ids])

(defn in-range?
  [i r]
  (def [start end] r)
  (and (>= i start) (<= i end)))

(defn solve-p1
  [ranges ids]
  (defn in-any-range?
    [i]
    (any? (map |(in-range? i $) ranges)))

  (length (filter in-any-range? ids)))

(defn range-length
  [r]
  (inc (- (get r 1) (get r 0))))

(defn solve-p2
  [ranges]
  (sort-by |(get $ 0) ranges)

  (def out @[])
  (var curr (get ranges 0))
  (loop [i :range [1 (length ranges)]]
    (def [s1 e1] curr)
    (def [s2 e2] (get ranges i))

    (cond
      # If ranges don't overlap, store current one and start from the next
      (> s2 e1) (do
                  (array/push out curr)
                  (set curr [s2 e2]))

      # If ranges overlap and the next one is longer, extend current one
      (> e2 e1) (do
                  (set curr [s1 e2]))))

  (array/push out curr)
  (apply + (map range-length out)))

(defn solve
  [part input]
  (def [ranges ids] (parse-input input))
  (case part
    :p1 (solve-p1 ranges ids)
    :p2 (solve-p2 ranges)))

(print (solve :p1 input))
(print (solve :p2 input))
