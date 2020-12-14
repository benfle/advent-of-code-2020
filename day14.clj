(ns day14
  (:require [clojure.string :as str]))

(defn mask-fn
  "Transform the mask string into a function to transform a long."
  [mask]
  (let [bit-mask #(-> mask
                      (str/escape (if % {\X \1} {\X \0}))
                      (Long/parseLong 2))
        zero-mask (bit-mask true)
        one-mask  (bit-mask false)]
    (fn [n]
      (-> n
          (bit-and zero-mask)
          (bit-or one-mask)))))

(assert (= 2r100 ((mask-fn "1X0") 2r001)))

(defn parse-instruction
  [s]
  (let [[left right] (map str/trim (str/split s #"="))]
    (if (= left "mask")
      [:mask right]
      (let [[_ _ addr] (re-find  #"(.+)\[(.+)\]" left)]
        [:store (Integer. addr) (Long. right)]))))

(def instructions
  (->> (slurp "day14.input")
       str/split-lines
       (map parse-instruction)))

(defn eval-1
  [state [operator & operands]]
  (case operator
    :mask (assoc state :mask (mask-fn (first operands)))
    :store (let [[addr val] operands]
             (assoc-in state [:memory addr] ((:mask state) val)))))

(def answer1 (->> instructions
                  (reduce eval-1 {:memory {}})
                  :memory
                  vals
                  (reduce +)))

(defn padded-binary-string
  [n]
  (str/escape (format "%36s" (Long/toBinaryString n))
              {\ \0}))

(defn mask-fn-2
  "Return a function that return all addresses for this mask and number."
  [mask]
  (fn [n]
    (let [binary (padded-binary-string n)]
      (map #(Long/parseLong (apply str %) 2)
           (reduce (fn [addresses i]
                     (let [next-bits (case (.charAt mask i)
                                       \0 [(.charAt binary i)]
                                       \1 [\1]
                                       \X [\0 \1])]
                       (vec
                        (for [address addresses
                              next-bit next-bits]
                          (conj address next-bit)))))
                   [[]]
                   (range (count binary)))))))

(defn eval-2
  [state [operator & operands]]
  (case operator
    :mask (assoc state :mask (mask-fn-2 (first operands)))
    :store (let [[addr val] operands]
             (reduce (fn [state address]
                       (assoc-in state [:memory address] val))
                     state
                     ((:mask state) addr)))))

(def answer2 (->> instructions
                  (reduce eval-2 {:memory {}})
                  :memory
                  vals
                  (reduce +)))
