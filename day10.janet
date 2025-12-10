(use "./common")

(def test-input `[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}`)

(def input (string/trim (slurp "inputs/day10.txt")))

(defn parse-input
  [input]
  (def grammar '{:lights (* "[" (<- (some (set ".#"))) "]")
                 :btn (* "(" (group (some (* (<- :d+) (? ",")))) ")")
                 :jolts (* "{" (group (some (* (<- :d+) (? ",")))) "}")
                 :main (* :lights :s+ (group (some (* :btn :s+))) :jolts)})

  (defn lights-to-bits
    [lights]
    (reduce
      (fn
        [acc c]
        (bor
          (blshift acc 1)
          (bool2num (= c (chr "#")))))
      0
      (reverse lights)))

  (map
    (fn
      [line]
      (let [[lights buttons jolts] (peg/match grammar line)]
        [(lights-to-bits lights)
         (map (fn [btns] (tuple ;(map scan-number btns))) buttons)
         (map scan-number jolts)]))
    (string/split "\n" (string/trim input))))

(defn flip-bits
  [n bits]
  (reduce
    (fn
      [acc b]
      (bxor acc (blshift 1 b)))
    n
    bits))

(defn fewest-total-presses
  [target btns]
  (def queue @[{:state 0
                :presses 0
                :prev []}])

  (var res nil)
  (while (and (not (empty? queue)) (nil? res))
    (def {:state state :presses presses :prev prev} (first queue))
    (array/remove queue 0)

    (each btn btns
      (when (not (has-value? prev btn))
        (let [new-state (flip-bits state btn)]
          (if (= new-state target)
            (set res (inc presses))
            (array/push queue {:state new-state
                               :presses (inc presses)
                               :prev (tuple/join prev [btn])}))))))
  res)

(defn solve-p1
  [machines]
  (sum (map |(let [[target btns _] $] (fewest-total-presses target btns)) machines)))

(defn solve
  [part input]
  (let [machines (parse-input input)]
    (case part
      :p1 (solve-p1 machines)
      :p2 nil)))

(print (solve :p1 input))
