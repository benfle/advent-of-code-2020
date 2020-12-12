(ns day12
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

(def instructions
  (->> "day12.input"
       slurp
       str/split-lines
       (map #(let [[_ action value] (re-find #"([NSEWLRF])(\d+)" %)]
               [(first action) (Integer. value)]))))

(def ^{:doc "Map instruction to rotation matrix in 2D."}
  rotation
  {[\L  90] [[ 0  1] [-1  0]]
   [\L 180] [[-1  0] [ 0 -1]]
   [\L 270] [[ 0 -1] [ 1  0]]
   [\R  90] [[ 0 -1] [ 1  0]]
   [\R 180] [[-1  0] [ 0 -1]]
   [\R 270] [[ 0  1] [-1  0]]})

(defn translation
  "Map instruction to translation vector in 2D."
  [[action value]]
  (case action
    \N [       0    value]
    \S [       0 (- value)]
    \E [   value        0]
    \W [(- value)       0]))

(defn move
  "Move the boat according to the instruction."
  [{:keys [direction] :as boat} [action value :as instruction]]
  (case action
    (\N \S \E \W) (update boat :position  matrix/add (translation instruction))
    (\L \R)       (update boat :direction matrix/mmul (rotation instruction))
    \F            (update boat :position  matrix/add (matrix/dot value direction))))

(defn manhattan-distance
  [[east north]]
  (+ (Math/abs east) (Math/abs north)))

(def initial-boat {:position  [0 0]
                   :direction [1 0]})

(def navigate reduce)

(def answer1 (manhattan-distance
              (:position (navigate move initial-boat instructions))))

(defn move-waypoint
  [{:keys [direction] :as boat} [action value :as instruction]]
  (case action
    (\N \S \E \W) (update boat :direction matrix/add (translation instruction))
    (\L \R)       (update boat :direction matrix/mmul (rotation instruction))
    \F            (update boat :position matrix/add (matrix/dot value direction))))

(def initial-boat-waypoint
  {:position [0 0]
   :direction [10 1]})

(def answer2 (manhattan-distance
              (:position (navigate move-waypoint initial-boat-waypoint instructions))))
