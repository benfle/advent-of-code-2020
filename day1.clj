(ns day1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def numbers
  (with-open [rdr (io/reader "./day1.input")]
    (->> (line-seq rdr)
         (map edn/read-string)
         set)))

(defn answer
  [numbers sum]
  (some #(let [other (- sum %)]
           (when (contains? (disj numbers %) other)
             (* % other)))
        numbers))

(def answer1 (answer numbers 2020))

(def answer2 (some #(some-> (answer (disj numbers %)
                                    (- 2020 %))
                            (* %))
                   numbers))
