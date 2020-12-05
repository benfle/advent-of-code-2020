(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn id
  [row col]
  (+ (* 8 row) col))

(defn boarding-pass
  [s]
  (let [[row-chars col-chars] (partition-all 7 s)
        row (Integer/parseInt (apply str (map {\F \0 \B \1} row-chars)) 2)
        col (Integer/parseInt (apply str (map {\L \0 \R \1} col-chars)) 2)]
    {:row row
     :col col
     :id (id row col)}))

(def boarding-passes
  (with-open [rdr (io/reader "./day5.input")]
    (->> (line-seq rdr)
         (map boarding-pass)
         doall)))

(def answer1 (->> boarding-passes (map :id) (apply max)))

(defn missing
  [boarding-passes]
  (let [seats-in-my-row (->> boarding-passes
                             (group-by :row)
                             (sort-by key)
                             ;; seat is not in the "first" row
                             (drop 1)
                             ;; seat is not in the "last" row
                             (drop-last 1)
                             ;; only 7 seats are taken on my row
                             (filter #(= 7 (count (val %))))
                             first
                             val)
        row (-> seats-in-my-row first :row)
        taken? (set (map :col seats-in-my-row))
        col (first (remove taken? (range 1 8)))]
    {:row row
     :col col
     :id (id row col)}))

(def answer2 (missing boarding-passes))
