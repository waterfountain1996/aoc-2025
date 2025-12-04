(use "./common")

(def test-input `..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.`)

(def input (string/trim (slurp "inputs/day04.txt")))

(defn parse-input
  [input]
  (string/split "\n" input))

(def neighbors [[-1 -1]
                [-1 0]
                [-1 1]
                [0 -1]
                [0 1]
                [1 -1]
                [1 0]
                [1 1]])

(defn num-adjacent
  [grid y x removed]
  (apply + (map
             (fn
               [[dy dx]]
               (def [ny nx] [(+ y dy) (+ x dx)])
               (def n (get-in grid [ny nx]))
               (bool2num (and (= n (chr "@")) (not (in removed [ny nx])))))
             neighbors)))

(defn solve-p1
  [grid]
  (var res 0)
  (loop [[y row] :pairs grid]
    (loop [[x col] :pairs row]
      (when (and (= col (chr "@")) (< (num-adjacent grid y x @{}) 4))
        (++ res))))
  res)

(defn solve-p2
  [grid]
  (var removed @{})
  (var keep-going true)
  (while keep-going
    (def removed2 @{})
    (loop [[y row] :pairs grid]
      (loop [[x col] :pairs row]
        (when (and (= col (chr "@")) (< (num-adjacent grid y x removed) 4) (not (in removed [y x])))
          (set (removed2 [y x]) true))))
    (when (empty? removed2) (set keep-going false))
    (set removed (merge removed removed2)))
  (length removed))

(defn solve
  [part input]
  (def grid (parse-input input))
  (case part
    :p1 (solve-p1 grid)
    :p2 (solve-p2 grid)))

(pp (solve :p1 input))
(pp (solve :p2 input))
