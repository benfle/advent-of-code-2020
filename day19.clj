(ns day19
  (:require [clojure.string :as str]))

(defn parse-rule
  [line]
  (let [[id clauses] (str/split line #":")
        clauses (str/trim clauses)]
    [(Integer. id)
     (if (#{"\"a\"" "\"b\""} clauses)
       [:terminal (second clauses)]
       [:non-terminal (->> (str/split clauses #"\|")
                           (map (fn [clause]
                                  (map #(Integer. %)
                                       (str/split (str/trim clause) #" ")))))])]))

(defn parse-input
  [s]
  (let [[rules message] (->> (str/split s #"\n\n")
                             (map #(str/split % #"\n")))]
    {:rules (into {} (map parse-rule rules))
     :messages message}))

(def input (parse-input (slurp "day19.input")))

(declare match)

(defn match-seq
  "Match the rules in sequence on the given message.

  Return a seq of matches."
  [rules message rule-ids]
  (reduce (fn [messages rule-id]
            (->> messages
                 (remove #{:error})
                 (mapcat #(match rules % rule-id))))
          [message]
          rule-ids))

(defn match-alt
  "A seq of matches for this set of alternatives."
  [rules message alts]
  (when (seq alts)
    (concat (match-seq rules message (first alts))
            (match-alt rules message (rest alts)))))

(defn match
  "A seq of matches for this message."
  [rules message n]
  (let [[type data] (rules n)]
    (case type
      :terminal
      (if (= data (first message))
        [(subs message 1)]
        [:error])

      :non-terminal
      (match-alt rules message data))))

(defn valid?
  [rules message n]
  (some #(and (string? %)
              (empty? %))
        (match rules message n)))

(def answer1 (->> (:messages input)
                  (filter #(valid? (:rules input) % 0))
                  count))

(defn update-input
  [input]
  (-> input
      (assoc-in [:rules 8] [:non-terminal '((42) (42 8))])
      (assoc-in [:rules 11] [:non-terminal '((42 31) (42 11 31))])))

(def updated-input (update-input input))

(def answer2 (->> (:messages updated-input)
                  (filter #(valid? (:rules updated-input) % 0))
                  count))
