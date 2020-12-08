(ns day8
  (:require [clojure.string :as str]))

(def instructions
  (->> (slurp "./day8.input")
       str/split-lines
       (map (fn [s]
              (let [[op arg] (str/split s #" ")]
                [(keyword op) (Integer. arg)])))
       (into [])))

(defn do-one-cycle
  [instructions state]
  (let [[op arg] (instructions (:pc state))]
    (case op
      :nop (update state :pc inc)
      :jmp (update state :pc + arg)
      :acc (-> state
               (update :acc + arg)
               (update :pc inc)))))

(defn run
  "Eval the instructions and return the program.

  Stops at the first loop or when the program reaches the end of the instructions."
  [instructions]
  (loop [{:keys [pc] :as state} {:pc 0
                                 :acc 0}
         visited? #{}]
    (cond
      (visited? pc)
      {:status :loop
       :state state}

      (<= (count instructions) pc)
      {:status :terminated
       :state state}

      :else
      (recur (do-one-cycle instructions state)
             (conj visited? pc)))))

(def answer1 (:acc (:state (run instructions))))

(def patched-instructions
  (->> instructions
       (map-indexed (fn [idx [op arg]]
                      (case op
                        :jmp (assoc-in instructions [idx 0] :nop)
                        :nop (assoc-in instructions [idx 0] :jmp)
                        nil)))
       (remove nil?)
       doall))

(def answer2 (some #(let [{:keys [status state]} (run %)]
                      (when (= :terminated status)
                        (:acc state)))
                   patched-instructions))
