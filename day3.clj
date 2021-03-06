(ns day3
  (:require [clojure.java.io :as io]))

(def tree-map
  (with-open [rdr (io/reader "./day3.input")]
    (->> (line-seq rdr)
         (mapv #(vec (seq %))))))

(defn cell-at
  "The content of the cell at the given coordinate.

  Handles the fact that the map repeats horizontally."
  [[x y]]
  (let [width (count (first tree-map))]
    (get-in tree-map [y (mod x width)])))

(defn tree?
  "Whether the cell is a tree."
  [cell]
  (= \# cell))

(defn trajectory
  "The seq of coordinates when following this slope.

  Stop before reaching the bottom."
  [[dx dy]]
  (map vector
       (map #(* dx %) (range))
       (map #(* dy %) (range (count tree-map)))))

(defn check-slope
  "The number of trees for this slope."
  [slope]
  (->> (trajectory slope)
       (filter #(tree? (cell-at %)))
       count))

(def answer1 (check-slope [3 1]))

(def slopes
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

(def answer2
  (->> slopes
       (map check-slope)
       (reduce *)))
