(use "./common")

(def test-input `aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out`)

(def test-input2 `svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out`)

(def input (string/trim (slurp "inputs/day11.txt")))

(defn parse-input
  [input]
  (def grammar '{:node (repeat 3 :w)
		 :main (* (<- :node) ":" :s (group (some (* (<- :node) (? :s)))))})

  (let [lines (string/split "\n" (string/trim input))
	edges (map |(peg/match grammar $) lines)
	ins (map |(keyword (get $ 0)) edges)
	outs (map |(map keyword (get $ 1)) edges)]
    (zipcoll ins outs)))

(defn solve-p1
  [edges]
  (defn count-paths
    [u t]
    (if (= u t)
      1
      (sum (map (fn [v] (memoize (count-paths v t))) (in edges u)))))
  (count-paths :you :out))

(defn solve-p2
  [edges]
  (defn count-paths
    [u t seen-dac seen-fft]
    (if (= u t)
      (bool2num (and seen-dac seen-fft))
      (sum
	(map
	  (fn
	    [v]
	    (memoize (count-paths v t (or seen-dac (= u :dac)) (or seen-fft (= u :fft)))))
	  (in edges u)))))
  (count-paths :svr :out false false))

(defn solve
  [part input]
  (let [edges (parse-input input)]
    (case part
      :p1 (solve-p1 edges)
      :p2 (solve-p2 edges))))

(print (solve :p1 input))
(print (solve :p2 input))
