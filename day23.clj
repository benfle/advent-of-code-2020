(ns day23)

(set! *warn-on-reflection* true)

;; Representing the cups as an array of integers
;; The first element is the value of the current cup.
;; For the rest, cups[i] = j means that the cup with value j follows
;; the cup with value i.

(def input [9 6 2 7 1 3 8 5 4])

(defn cups
  [input]
  (let [a (int-array (inc (count input)))]
    (aset a 0 ^int (first input))
    (doseq [idx (range (dec (count input)))]
      (aset a ^int (input idx) ^int (input (inc idx))))
    (aset a ^int (input (dec (count input))) ^int (first input))
    a))

(defn rights
  "All cups to the right of the given cup."
  [^ints cups n]
  (->> (iterate #(aget cups %) n)
       rest
       (take (- (count cups) 2))))

(defn do-one-move
  [^ints cups]
  (let [current (aget cups 0)
        [c1 c2 c3 c4] (take 4 (rights cups current))
        destination (loop [n (dec current)]
                      (cond
                        (zero? n)             (recur (dec (alength cups)))
                        (not (#{c1 c2 c3} n)) n
                        :else                 (recur (dec n))))]
    (aset cups current ^int c4)
    (aset cups 0 ^int c4)
    (aset cups ^int c3 (aget cups destination))
    (aset cups destination ^int c1)))

(def answer1 (let [cups (cups input)]
               (dotimes [_ 100] (do-one-move cups))
               (apply str (rights cups 1))))

(def answer2 (time (let [cups (cups (into input (range (inc (reduce max input)) 1000001)))]
                     (dotimes [_ 10000000] (do-one-move cups))
                     (->> (rights cups 1)
                          (take 2)
                          (reduce *)))))
