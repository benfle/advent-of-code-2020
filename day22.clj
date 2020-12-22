(ns day22
  (:require [clojure.string :as str]))

(defn fixed-point
  [improve start]
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate improve start)))

(def input
  (let [[p1 p2] (str/split (slurp "day22.input") #"\n\n")
        parse-hand (fn [hand]
                     (->> (str/split hand #"\n")
                          (rest)
                          (mapv #(Integer. %))))]
    {:p1 (parse-hand p1)
     :p2 (parse-hand p2)}))

(defn play-one-round
  [{:keys [p1 p2] :as game}]
  (cond
    (empty? p2) (assoc game :winner :p1)
    (empty? p1) (assoc game :winner :p2)
    :else       (let [c1 (first p1) xs1 (subvec p1 1)
                      c2 (first p2) xs2 (subvec p2 1)]
                  (if (< c2 c1)
                    {:p1 (into xs1 [c1 c2])
                     :p2 xs2}
                    {:p1 xs1
                     :p2 (into xs2 [c2 c1])}))))

(defn play
  [game]
  (fixed-point play-one-round game))

(defn score
  [{:keys [winner] :as game}]
  (->> (winner game)
       (reverse)
       (map * (iterate inc 1))
       (reduce +)))

(def answer1 (score (play input)))

(declare play-recursive)

(defn play-one-recursive-round
  [{:keys [p1 p2 previous-rounds] :as game}]
  (cond
    (contains? previous-rounds [p1 p2])
    (assoc game :winner :p1)

    (not (seq p1))
    (assoc game :winner :p2)

    (not (seq p2))
    (assoc game :winner :p1)

    :else
    (let [[c1 & xs1] p1
          [c2 & xs2] p2
          winner (if (and (< c1 (count p1))
                          (< c2 (count p2)))
                   (:winner (play-recursive {:p1 (subvec p1 1 (inc c1))
                                             :p2 (subvec p2 1 (inc c2))}))
                   (if (< c1 c2) :p2 :p1))]
      (assoc (case winner
               :p1 {:p1 (vec (concat xs1 [c1 c2]))
                    :p2 (vec xs2)}
               :p2 {:p1 (vec xs1)
                    :p2 (vec (concat xs2 [c2 c1]))})
             :previous-rounds
             (conj previous-rounds [p1 p2])))))

(defn play-recursive
  [game]
  (fixed-point play-one-recursive-round (assoc game :previous-rounds #{})))

(def answer2 (score (play-recursive input)))
