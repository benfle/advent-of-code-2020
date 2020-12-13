(ns day13
  (:require [clojure.string :as str]))

(let [lines (str/split-lines (slurp "day13.input"))]
  (def ts (Integer. (first lines)))
  (def ids (->> (str/split (second lines) #",")
                (mapv #(when (not (= "x" %)) (Integer. %))))))

(defn earliest-bus
  [ts ids]
  (loop [ts ts]
    (or (some (fn [id]
                (when (and id (zero? (mod ts id)))
                  [id ts]))
              ids)
        (recur (inc ts)))))

(def answer1 (let [[bus-id bus-time] (earliest-bus ts ids)]
               (* bus-id (- bus-time ts))))

;; This is the Chinese Remainder Theorem
;; The naive implementation will not work.

;; Efficient implementation taken from:
;; https://www.geeksforgeeks.org/chinese-remainder-theorem-set-2-implementation/?ref=lbp

(defn chinese-remainder-theorem
  "Implements the chinese remainder theorem.

  Return the minimum x such that:
  x % pairs[0][0] = pairs[0][1]
  x % pairs[1][0] = pairs[1][1]
  ...

  Only works if the elements of nums are pairwise coprime
  (gcd for every pair is 1) but this is not checked."
  [pairs]
  (let [prod (reduce * (map first pairs))]
    (mod (reduce (fn [res [num rem]]
                   (let [pp (quot prod num)]
                     (+ res (* rem
                               (.modInverse (biginteger pp)
                                            (biginteger num))
                               pp))))
                 0
                 pairs)
         prod)))

(defn- pairs
  "Transform the vector of ids into a seq of pairs that
  can be used with `chinese-remainder-theorem`."
  [ids]
  (->> ids
       (map-indexed (fn [idx id]
                      (when id
                        [id (if (zero? idx) 0 (- id idx))])))
       (remove nil?)))

(def answer2 (chinese-remainder-theorem (pairs ids)))
