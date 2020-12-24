(ns day24
  (:require [clojure.string :as str]))

(def instructions
  (map #(re-seq #"e|se|sw|w|nw|ne" %)
       (str/split (slurp "day24.input") #"\n")))

(defn move
  [[x y] direction]
  (case direction
    "e"  [(inc x)        y]
    "w"  [(dec x)        y]
    "ne" [(+ x 0.5) (inc y)]
    "se" [(+ x 0.5) (dec y)]
    "nw" [(- x 0.5) (inc y)]
    "sw" [(- x 0.5) (dec y)]))

(def tiling
  (->> instructions
       (map #(reduce move [0 0] %))
       (frequencies)
       (filter #(odd? (val %)))
       (map first)
       set))

(def answer1 (count tiling))

(defn neighbors
  [pos]
  (map #(move pos %) ["e" "se" "sw" "w" "nw" "ne"]))

(defn do-one-day
  [tiling]
  (->> tiling
       (mapcat neighbors)
       (set)
       (map (fn [pos]
              (let [color (if (contains? tiling pos) :black :white)
                    cnt (count (keep tiling (neighbors pos)))]
                (case color
                  :black (when (not (or (zero? cnt) (< 2 cnt))) pos)
                  :white (when (= 2 cnt) pos)))))
       (remove nil?)
       set))

(def answer2
  (time
   (->> (iterate do-one-day tiling)
        (take 101)
        (last)
        (count))))
