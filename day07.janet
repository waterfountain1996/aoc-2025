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

(defn run
  [grid row col cache]
  (def tail (array/slice grid (inc row)))
  (def offset (find-index |(= (get $ col) SPLITTER) tail))

  (if-not (nil? offset)
    (do
      (def next-row (+ row offset 1))
      (when (nil? (in cache [next-row col]))
        (set (cache [next-row col]) true)
        (run grid next-row (dec col) cache)
        (run grid next-row (inc col) cache)))))

(defn solve-p1
  [grid]
  (def col (index-of (chr "S") (first grid)))
  (def cache @{})
  (run grid 0 col cache)
  (length cache))

(defn run2
  [grid row col cache]
  (def tail (array/slice grid (inc row)))
  (def offset (find-index |(= (get $ col) SPLITTER) tail))

  (if (nil? offset)
    1
    (do
      (def next-row (+ row offset 1))

      (when (nil? (in cache [next-row (dec col)]))
        (set (cache [next-row (dec col)]) (run2 grid next-row (dec col) cache)))

      (when (nil? (in cache [next-row (inc col)]))
        (set (cache [next-row (inc col)]) (run2 grid next-row (inc col) cache)))

      (+
        (in cache [next-row (dec col)])
        (in cache [next-row (inc col)])))))

(defn solve-p2
  [grid]
  (def col (index-of (chr "S") (first grid)))
  (def cache @{})
  (run2 grid 0 col cache))

(defn solve
  [part input]
  (def grid (parse-input input))
  (case part
    :p1 (solve-p1 grid)
    :p2 (solve-p2 grid)))

(print (solve :p1 input))
(print (solve :p2 input))
