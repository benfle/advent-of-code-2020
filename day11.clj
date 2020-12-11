(ns day11
  (:require [clojure.string :as str]))

(def layout
  (->> "day11.input"
       slurp
       str/split-lines
       (mapv #(vec (seq %)))))

(def floor         \.)
(def empty-seat    \L)
(def occupied-seat \#)

(defn adjacent-positions
  [[row col]]
  [[(dec row) (dec col)] [(dec row) col] [(dec row) (inc col)]
   [     row  (dec col)]                 [     row  (inc col)]
   [(inc row) (dec col)] [(inc row) col] [(inc row) (inc col)]])

(defn adjacent-seats
  [layout [row col]]
  (->> (adjacent-positions [row col])
       (map #(get-in layout %))
       (remove #(or (nil? %) (= floor %)))))

(defn apply-rules
  [layout position]
  (let [cell (get-in layout position)
        adjacent-seats (adjacent-seats layout position)]
    (cond
      (and (= empty-seat cell)
           (every? #{empty-seat} adjacent-seats))
      occupied-seat

      (and (= occupied-seat cell)
           (<= 4 (->> adjacent-seats
                      (filter #{occupied-seat})
                      count)))
      empty-seat

      :else
      cell)))

(defn round
  "A function to do one round with the given rules."
  [apply-rules]
  (fn [layout]
    (mapv (fn [row]
            (mapv (fn [col]
                    (apply-rules layout [row col]))
                  (range (count (first layout)))))
          (range (count layout)))))

(defn fixed-point
  [f val]
  (reduce #(if (= %1 %2)
             (reduced %1)
             %2)
          (iterate f val)))

(def answer1 (->> (fixed-point (round apply-rules) layout)
                  (apply concat)
                  (filter #{occupied-seat})
                  count))

(defn visible-seat
  "The visible seat from the position in the given slope."
  [layout [row col] [drow dcol]]
  (loop [n 1]
    (let [cell (get-in layout [(+ row (* n drow))
                               (+ col (* n dcol))])]
      (if (= floor cell)
        (recur (inc n))
        cell))))

(defn visible-seats
  "All visible seats from this position (at most 8)."
  [layout [row col]]
  (->> [[-1 -1] [-1 0] [-1 1]
        [ 0 -1]        [ 0 1]
        [ 1 -1] [ 1 0] [ 1 1]]
       (map #(visible-seat layout [row col] %))
       (remove nil?)))

(defn apply-rules-2
  [layout position]
  (let [cell (get-in layout position)
        visible-seats (visible-seats layout position)]
    (cond
      (and (= empty-seat cell)
           (every? #{empty-seat} visible-seats))
      occupied-seat

      (and (= occupied-seat cell)
           (<= 5 (->> visible-seats
                      (filter #{occupied-seat})
                      count)))
      empty-seat

      :else
      cell)))

(def answer2 (->> (fixed-point (round apply-rules-2) layout)
                  (apply concat)
                  (filter #{occupied-seat})
                  count))
