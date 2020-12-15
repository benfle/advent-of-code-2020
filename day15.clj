(ns day15)

(def starting-numbers [13 16 0 12 15 1])

(defn next-turn
  [{:keys [turn number spoken]}]
  {:turn (inc turn)
   :number (if (contains? spoken number)
             (- turn (spoken number))
             0)
   :spoken (assoc spoken number turn)})

(defn turns
  [starting-numbers]
  (iterate next-turn {:turn (count starting-numbers)
                      :number (last starting-numbers)
                      :spoken (->> (iterate inc 1)
                                   (map vector (butlast starting-numbers))
                                   (into {}))}))

(defn spoken-number
  [n]
  (some (fn [{:keys [turn number]}]
          (when (= n turn)
            number))
        (turns starting-numbers)))

(def answer1 (spoken-number 2020))

(def answer2 (time (spoken-number 30000000)))
