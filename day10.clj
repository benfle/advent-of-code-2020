(ns day10
  (:require [clojure.string :as str]))

(def adapters
  (let [adapters (->> (slurp "./day10.input")
                      str/split-lines
                      (map #(Integer. %))
                      sort
                      (into [0]))]
    (conj adapters (+ (last adapters) 3))))

(def answer1
  (let [freqs (frequencies
               (map #(- %2 %1) adapters (rest adapters)))]
    (* (freqs 1)
       (freqs 3))))

(defn count-arrangements
  []
  (reduce (fn [cnts n]
            (assoc cnts n (+ (get cnts (+ n 1) 0)
                             (get cnts (+ n 2) 0)
                             (get cnts (+ n 3) 0))))
          {(last adapters) 1}
          (rest (reverse adapters))))

(def answer2 ((count-arrangements) 0))
