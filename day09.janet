(use "./common")

(def test-input `7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3`)

(def input (string/trim (slurp "inputs/day09.txt")))

(defn parse-input
  [input]
  (let [trimmed (string/trim input)
        lines (string/split "\n" trimmed)
        tiles (map |(let [[x y] (string/split "," $)] [(scan-number x) (scan-number y)]) lines)]
    tiles))


(defn combinations
  [tiles]
  (seq [i :range [0 (length tiles)]
        j :range [(inc i) (length tiles)]]
    [(get tiles i) (get tiles j)]))

(defn area
  [[r1 r2]]
  (let [[x1 y1] r1
        [x2 y2] r2
        a (inc (math/abs (- x2 x1)))
        b (inc (math/abs (- y2 y1)))]
    (* a b)))

(defn solve-p1
  [tiles]
  (let [rects (combinations tiles)
        areas (map area rects)]
    (max-of areas)))

(defn solve
  [part input]
  (let [tiles (parse-input input)]
    (case part
      :p1 (solve-p1 tiles)
      :p2 nil)))

(print (solve :p1 input))
