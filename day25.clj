(ns day25)

(set! *unchecked-math* :warn-on-boxed)

(def pk1 10441485)
(def pk2 1004920)

(defn transforms
  [^long subject-number]
  (iterate #(rem (* ^long % subject-number) 20201227)
           1))

(defn transform
  [subject-number ^long step-size]
  (->> (transforms subject-number)
       (take (inc step-size))
       (last)))

(defn guess-loop-size
  [subject-number public-key]
  (->> (transforms subject-number)
       (map vector (range))
       (some (fn [[idx n]]
               (when (= n public-key)
                 idx)))))

(def ek1 (transform pk1 (guess-loop-size 7 pk2)))
(def ek2 (transform pk2 (guess-loop-size 7 pk1)))

(assert (= ek1 ek2))

(def answer1 ek1)
