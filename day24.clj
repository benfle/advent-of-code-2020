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
       (map (fn [[pos cnt]]
              [pos (if (odd? cnt) :black :white)]))
       (into {})))

(def answer1 (count (filter #(= :black (val %)) tiling)))

(defn neighbors
  [pos]
  (map #(move pos %) ["e" "se" "sw" "w" "nw" "ne"]))

(defn do-one-day
  [tiling]
  (->> (keys tiling)
       (mapcat neighbors)
       set
       (map (fn [pos]
              (let [color (get tiling pos :white)
                    cnt (->> (neighbors pos)
                             (map tiling)
                             (filter #{:black})
                             (count))]
                [pos (cond
                       (and (= color :black) (or (zero? cnt) (< 2 cnt))) :white
                       (and (= color :white) (= 2 cnt))                  :black
                       :else                                             color)])))
       (into {})))

(def answer2
  (time
   (->> (iterate do-one-day tiling)
        (take 101)
        (last)
        (filter #(= :black (val %)))
        (count))))
