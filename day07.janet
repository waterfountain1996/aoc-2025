(use "./common")

(def test-input `.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............`)


(def input (string/trim (slurp "inputs/day07.txt")))

(defn parse-input
  [input]
  (map string/trim (string/split "\n" input)))

(def SPLITTER (chr "^"))

(defn count-splits
  [grid row col cache]
  (if-let [offset (find-index |(= (get $ col) SPLITTER) (array/slice grid row))]
    (let [next-row (+ row offset)
          key [next-row col]
          c1 (dec col)
          c2 (inc col)]
      (when (not (has-key? cache key))
        (set (cache key) :seen)
        (count-splits grid next-row c1 cache)
        (count-splits grid next-row c2 cache))))
  (length cache))

(defn count-quantum-splits
  [grid row col]
  (if-let [offset (find-index |(= (get $ col) SPLITTER) (array/slice grid row))]
    (let [next-row (+ row offset)
          c1 (dec col)
          c2 (inc col)]
      (+
        (memoize (count-quantum-splits grid next-row c1))
        (memoize (count-quantum-splits grid next-row c2))))
    1))

(defn solve
  [part input]
  (def grid (parse-input input))
  (case part
    :p1 (count-splits grid 0 (index-of (chr "S") (first grid)) @{})
    :p2 (count-quantum-splits grid 0 (index-of (chr "S") (first grid)))))

(print (solve :p1 input))
(print (solve :p2 input))
