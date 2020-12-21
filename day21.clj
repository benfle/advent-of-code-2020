(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (->> (str/split (slurp "day21.input") #"\n")
       (map (fn [line]
              (let [[ingredients allergens] (str/split line #" \(contains")]
                {:ingredients (-> ingredients
                                  (str/split #" ")
                                  (set))
                 :allergens (-> allergens
                                (str/replace #"[,)]" "")
                                (str/trim)
                                (str/split #" ")
                                (set))})))
       (doall)))

(defn allergen->ingredients
  [ingredients-lists]
  (->> ingredients-lists
       (reduce (fn [m {:keys [ingredients allergens]}]
                 (reduce (fn [m allergen]
                           (update m allergen (fnil conj []) ingredients))
                         m
                         allergens))
               {})
       (map (fn [[allergen ingredients]]
              [allergen (apply set/intersection
                               (map set ingredients))]))
       (into {})))

(defn ingredient->allergen
  [allergen->ingredients]
  (let [improve (fn [ingredient->allergen]
                  (reduce (fn [ingredient->allergen [allergen ingredients]]
                            (let [ingredients (remove #(contains? ingredient->allergen %)
                                                      ingredients)]
                              (if (= 1 (count ingredients))
                                (assoc ingredient->allergen (first ingredients) allergen)
                                ingredient->allergen)))
                          ingredient->allergen
                          allergen->ingredients))]
    (reduce #(if (= %1 %2) (reduced %1) %2)
            (iterate improve {}))))

(defn count-ingredients
  [ingredients-lists ingredients]
  (->> ingredients-lists
       (map #(count (set/intersection ingredients (:ingredients %))))
       (reduce +)))

(defn all-ingredients
  [ingredients-lists]
  (->> ingredients-lists
       (mapcat :ingredients)
       set))

(def answer1 (->> input
                  (allergen->ingredients)
                  (ingredient->allergen)
                  (keys)
                  (set)
                  (set/difference (all-ingredients input))
                  (count-ingredients input)))

(def answer2 (->> input
                  (allergen->ingredients)
                  (ingredient->allergen)
                  (sort-by #(val %))
                  (map first)
                  (str/join ",")))
