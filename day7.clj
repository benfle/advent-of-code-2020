(ns day7
  (:require [clojure.string :as str]))

(defn parse-rule
  [line]
  (let [tokens (->> (str/split line #"[\s\.,]")
                    (remove str/blank?)
                    (remove #{"contain" "bag" "bags"}))
        color (vec (take 2 tokens))
        contents (->> tokens
                      (drop 2)
                      (partition 3)
                      (map (fn [[n q1 q2]]
                             [[q1 q2] (Integer. n)]))
                      (into {}))]
    [color contents]))

(def rules
  (->> (slurp "./day7.input")
       str/split-lines
       (map parse-rule)
       (into {})))

(def direct-containers
  (reduce (fn [containers [container contents]]
            (reduce (fn [containers [color _]]
                      (update containers color (fnil conj #{}) container))
                    containers
                    contents))
          {}
          rules))

(defn containers
  [color]
  (->> (direct-containers color)
       (mapcat #(into [%] (containers %)))
       set))

(def answer1 (count (containers ["shiny" "gold"])))

(defn count-bags
  [color]
  (inc (reduce (fn [s [color n]]
                 (+ s (* n (count-bags color))))
               0
               (rules color))))

(def answer2 (dec (count-bags ["shiny" "gold"])))
