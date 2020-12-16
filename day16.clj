(ns day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-range
  "Return a function to check if a number is in the given range (both ends included)."
  [s]
  (let [[lo hi] (map #(Integer. %) (str/split s #"-"))]
    (fn [n]
      (<= lo n hi))))

(defn parse-specs
  "Return a map { field -> predicate }.

  The predicate is a function checking that a number is valid for this field."
  [lines]
  (->> lines
       (map #(let [[k ranges] (map str/trim (str/split % #":"))
                   [in-range-1? in-range-2?] (map parse-range (str/split ranges #" or "))]
               [k (fn [n]
                    (or (in-range-1? n)
                        (in-range-2? n)))]))
       (into {})))

(defn parse-ticket
  "The vec of numbers on this ticket."
  [line]
  (mapv #(Integer. %) (str/split line #",")))

(def input
  (let [[specs [_ ticket] [_ & nearby-tickets]] (->> (str/split (slurp "day16.input") #"\n\n")
                                                     (map #(str/split % #"\n")))]
    {:specs (parse-specs specs)
     :ticket (parse-ticket ticket)
     :nearby-tickets (map parse-ticket nearby-tickets)}))

(defn valid?
  "A number is valid if it conforms to at least one spec."
  [specs n]
  (some #(% n)
        (vals specs)))

(defn error-rate
  "The sum of the invalid numbers on this ticket."
  [specs ticket]
  (->> ticket
       (filter #(not (valid? specs %)))
       (reduce +)))

(def answer1 (->> (:nearby-tickets input)
                  (map #(error-rate (:specs input) %))
                  (reduce +)))

(defn possible-fields
  "The set of possible fields for this number."
  [specs n]
  (->> specs
       (filter (fn [[field predicate]] (predicate n)))
       (map first)
       set))

(defn find-positions
  "Return a map position -> field.

  Assumes the problem has a unique solution."
  [{:keys [specs ticket nearby-tickets]}]
  (->> (range (count ticket))
       (map (fn [position]
              [position
               (->> nearby-tickets
                    (filter (fn [ticket]
                              (every? #(valid? specs %) ticket)))
                    (map #(% position))
                    (reduce (fn [valids n]
                              (set/intersection valids
                                                (possible-fields specs n)))
                            (set (map first specs))))]))
       (sort-by #(count (second %)))
       (reduce (fn [assignments [position candidates]]
                 (->> candidates
                      (remove (set (vals assignments)))
                      first
                      (assoc assignments position)))
               {})))

(def answer2 (->> (find-positions input)
                  (filter #(str/starts-with? (val %) "departure"))
                  (map (fn [[position _]]
                         (get-in input [:ticket position])))
                  (reduce *)))
