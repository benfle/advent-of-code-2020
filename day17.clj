(ns day17
  (:require [clojure.string :as str]))

(defn initial-state
  "The initial state for the given number of dimensions."
  [n]
  (let [lines (str/split (slurp "day17.input") #"\n")]
    (set
     (for [y (range (count lines))
           x (range (count (first lines)))
           :when (= \# (get-in lines [y x]))]
       (into [x y] (repeat (- n 2) 0))))))

(defn neighbor-coordinates
  "The coordinates of the neighbors."
  [coord]
  (loop [[hd & tl] coord
         neighbors [[]]]
    (if (not hd)
      (remove #(= coord %) neighbors)
      (recur tl
             (for [d [-1 0 1]
                   coord neighbors]
               (conj coord (+ d hd)))))))

(defn active?
  [state coord]
  (contains? state coord))

(defn active-neighbors
  "The coordinates of the active neighbors."
  [state coord]
  (filter #(active? state %)
          (neighbor-coordinates coord)))

(defn all-neighbor-coordinates
  "All the coordinate to consider when doing a cycle.

  This is the set of all the neighbors of currently active cubes."
  [state]
  (->> state
       (mapcat neighbor-coordinates)
       set))

(defn next-cube-active?
  "Whether the next cube at these coordinates will be active."
  [state coord]
  (if (active? state coord)
    (#{2 3} (count (active-neighbors state coord)))
    (= 3 (count (active-neighbors state coord)))))

(defn do-one-cycle
  "Run one cycle on this state and return the next one."
  [state]
  (reduce (fn [next-state coord]
            (if (next-cube-active? state coord)
              (conj next-state coord)
              next-state))
          #{}
          (all-neighbor-coordinates state)))

(def answer1 (->> (iterate do-one-cycle (initial-state 3))
                  (take 7)
                  last
                  count))

(def answer2 (->> (iterate do-one-cycle (initial-state 4))
                  (take 7)
                  last
                  count))
