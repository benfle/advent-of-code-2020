(ns day2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def passwords
  (with-open [rdr (io/reader "./day2.input")]
    (->> (line-seq rdr)
         (map (fn [line]
                (if-let [[_ lo hi letter password]
                         (re-matches #"(\d+)-(\d+)\s+([a-z]+):\s+([a-z]+)"
                                     line)]
                  [(edn/read-string lo)
                   (edn/read-string hi)
                   (first letter)
                   password]
                  (throw (ex-info "Error reading password."
                                  {:line line})))))
         doall)))

(defn valid-password?
  [[lo hi letter password]]
  (when-let [frequency ((frequencies (seq password)) letter)]
    (<= lo frequency hi)))

(def answer1 (count (filter valid-password? passwords)))

(defn new-valid-password?
  [[lo hi letter password]]
  (let [letters (into [] password)]
    (when (<= hi (count letters))
      (if (= letter (letters (dec lo)))
        (not (= letter (letters (dec hi))))
        (= letter (letters (dec hi)))))))

(def answer2 (count (filter new-valid-password? passwords)))
