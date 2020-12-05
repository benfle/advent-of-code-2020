(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn seat-id
  [s]
  (-> s
      (str/escape {\F \0 \B \1
                   \L \0 \R \1})
      (Integer/parseInt 2)))

(def seat-ids
  (with-open [rdr (io/reader "./day5.input")]
    (->> (line-seq rdr)
         (map seat-id)
         doall)))

(def answer1 (apply max seat-ids))

(defn missing
  [seat-ids]
  (let [taken? (set seat-ids)]
    (->> (range (apply min seat-ids)
                (apply max seat-ids))
         (remove taken?)
         first)))

(def answer2 (missing seat-ids))
