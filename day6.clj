(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def answers
  (map (fn [group]
         (map #(set (seq %))
              (str/split group #"\n")))
       (str/split (slurp "./day6.input") #"\n\n")))

(def answer1 (->> answers
                  (map #(count (apply set/union %)))
                  (reduce +)))

(def answer2 (->> answers
                  (map #(count (apply set/intersection %)))
                  (reduce +)))
