(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-ingredients-list
  [line]
  (let [[ingredients allergens] (str/split line #"contains")]
    {:ingredients (set (re-seq #"\w+" ingredients))
     :allergens (set (re-seq #"\w+" allergens))}))

(def ingredients-lists
  (->> (str/split (slurp "day21.input") #"\n")
       (map parse-ingredients-list)
       (doall)))

(defn allergen->ingredients
  [ingredients-lists]
  (->> ingredients-lists
       (reduce (fn [m {:keys [ingredients allergens]}]
                 (merge-with concat m (zipmap allergens (repeat [ingredients]))))
               {})
       (map (fn [[allergen ingredients]]
              [allergen (apply set/intersection
                               (map set ingredients))]))
       (into {})))

(defn fixed-point
  [improve start]
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate improve start)))

(defn ingredient->allergen
  [ingredients-lists]
  (let [improve (fn [ingredient->allergen]
                  (reduce (fn [ingredient->allergen [allergen ingredients]]
                            (let [ingredients (remove #(contains? ingredient->allergen %)
                                                      ingredients)]
                              (if (= 1 (count ingredients))
                                (assoc ingredient->allergen (first ingredients) allergen)
                                ingredient->allergen)))
                          ingredient->allergen
                          (allergen->ingredients ingredients-lists)))]
    (fixed-point improve {})))

(def answer1
  (count (remove (set (keys (ingredient->allergen ingredients-lists)))
                 (mapcat :ingredients ingredients-lists))))

(def answer2
  (->> (ingredient->allergen ingredients-lists)
       (sort-by val)
       (map first)
       (str/join ",")))
