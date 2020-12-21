(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def ingredients-lists
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

(defn allergen->ingredientsg
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
  (fixed-point
   (fn [ingredient->allergen]
     (reduce (fn [ingredient->allergen [allergen ingredients]]
               (let [ingredients (remove #(contains? ingredient->allergen %)
                                         ingredients)]
                 (if (= 1 (count ingredients))
                   (assoc ingredient->allergen (first ingredients) allergen)
                   ingredient->allergen)))
             ingredient->allergen
             (allergen->ingredients ingredients-lists)))
   {}))

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

(def answer1 (->> ingredients-lists
                  (ingredient->allergen)
                  (keys)
                  (set)
                  (set/difference (all-ingredients input))
                  (count-ingredients input)))

(def answer2 (->> ingredients-lists
                  (ingredient->allergen)
                  (sort-by #(val %))
                  (map first)
                  (str/join ",")))
