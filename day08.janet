(import spork/test)
(use "./common")

(def test-input `162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689`)

(def input (string/trim (slurp "inputs/day08.txt")))

(defn parse-input
  [input]
  (let [trimmed (string/trim input)
        lines (string/split "\n" trimmed)
        grammar (peg/compile '(repeat 3 (* (<- :d+) (? ","))))
        boxes (map |(peg/match grammar $) lines)]
    (map |(tuple ;(map scan-number $)) boxes)))

(defn squared [x] (math/pow x 2))

(defn euclidean-distance
  [p q]
  (math/sqrt (sum (map |(squared (- (get p $) (get q $))) (range 3)))))

(defn combinations
  [boxes]
  (seq [i :range [0 (length boxes)]
        j :range [(inc i) (length boxes)]]
    [(get boxes i) (get boxes j)]))

(defn solve-p1
  [boxes junctions]
  (let [circuits (map |(table $ true) boxes)
        circuit-map (zipcoll boxes (range (length boxes)))]
    (each [p q] junctions
      (let [i (in circuit-map p)
            j (in circuit-map q)]
        (if-not (= i j)
          (let [c1 (in circuits i)
                c2 (in circuits j)
                merged (merge c1 c2)]
            (set (circuits i) nil)
            (set (circuits j) nil)
            (array/push circuits merged)
            (eachk key merged
              (set (circuit-map key) (dec (length circuits))))))))
    (let [cs (filter truthy? circuits)
          ss (sort-by |(- (length $)) cs)
          head (array/slice ss 0 3)]
      (product (map length head)))))

(defn solve-p2
  [boxes junctions]
  (let [circuits (map |(table $ true) boxes)
        circuit-map (zipcoll boxes (range (length boxes)))]
    (var res nil)
    (each [p q] junctions
      (let [i (in circuit-map p)
            j (in circuit-map q)]
        (if-not (= i j)
          (let [c1 (in circuits i)
                c2 (in circuits j)
                merged (merge c1 c2)]
            (set (circuits i) nil)
            (set (circuits j) nil)
            (array/push circuits merged)
            (eachk key merged
              (set (circuit-map key) (dec (length circuits))))
            (if (= (length (filter |(not (nil? $)) circuits)) 1)
              (set res (* (get p 0) (get q 0))))))))
    res))

(test/start-suite :main)
(let [boxes (parse-input test-input)
      junctions (combinations boxes)
      sorted-junctions (sort-by |(euclidean-distance (get $ 0) (get $ 1)) junctions)
      head (array/slice sorted-junctions 0 10)]

  (let [want 40
        got (solve-p1 boxes head)]
    (test/assert (= got want) (string/format "expected %d, got %d" want got)))

  (let [want 25272
        got (solve-p2 boxes junctions)]
    (test/assert (= got want) (string/format "expected %d, got %d" want got))))
(test/end-suite)

(defn solve
  [part input]
  (let [boxes (parse-input input)
        junctions (combinations boxes)
        sorted-junctions (sort-by |(euclidean-distance (get $ 0) (get $ 1)) junctions)]
    (case part
      :p1 (solve-p1 boxes (array/slice sorted-junctions 0 1000))
      :p2 (solve-p2 boxes sorted-junctions))))

(pp (solve :p1 input))
(pp (solve :p2 input))
