(ns day9
  (:require [clojure.string :as str]))

(def numbers
  (->> (slurp "./day9.input")
       str/split-lines
       (map #(Long. %))
       (into [])))

(defn sums
  [coll]
  (set
   (for [i (range 0 (count coll))
         j (range (inc i) (count coll))]
     (+ (coll i) (coll j)))))

(defn check
  [idx]
  (or (< idx 25)
      (contains? (sums (subvec numbers (- idx 25) idx))
                 (numbers idx))))

(def answer1 (some #(when (not (check %))
                      (numbers %))
                   (range (count numbers))))

(defn contiguous-set
  ([sum]
   (contiguous-set sum 0))
  ([sum idx]
   (loop [idx idx
          size 2]
     (let [candidate (subvec numbers idx (+ idx size))
           candidate-sum (reduce + candidate)]
       (cond
         (= sum candidate-sum) candidate
         (< sum candidate-sum) (recur (inc idx) 2)
         :else                 (recur idx (inc size)))))))

(def answer2 (let [set (contiguous-set answer1)]
               (+ (apply min set)
                  (apply max set))))
