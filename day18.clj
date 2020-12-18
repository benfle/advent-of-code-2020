(ns day18
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(def homework
  (->> (slurp "day18.input")
       str/split-lines
       (map #(str/replace % #" " ""))))

(def parse1
  (insta/parser
   "expr      = number | operation | paren
    operation = expr op ( number | paren )
    paren     = '(' expr ')'
    number    = #'[0-9]'
    op        = #'[+*]'"))

(def impl
  {"+" +
   "*" *})

(defmulti ev1 first)

(defmethod ev1 :expr      [[_ expr]]              (ev1 expr))
(defmethod ev1 :operation [[_ left [_ op] right]] ((impl op) (ev1 left) (ev1 right)))
(defmethod ev1 :paren     [[_ _ expr _]]          (ev1 expr))
(defmethod ev1 :number    [[_ s]]                 (Integer. s))

(def answer1 (->> homework
                  (map (comp ev1 parse1))
                  (reduce +)))

(def parse2
  (insta/parser
   "expr    = product
    product = sum '*' product | sum
    sum     = value '+' sum | value
    value   = number | paren
    paren   = '(' expr ')'
    number  = #'[0-9]'"))

(defmulti ev2 first)

(defmethod ev2 :expr    [[_ expr]]          (ev2 expr))
(defmethod ev2 :product [[_ left op right]] (if op (* (ev2 left) (ev2 right)) (ev2 left)))
(defmethod ev2 :sum     [[_ left op right]] (if op (+ (ev2 left) (ev2 right)) (ev2 left)))
(defmethod ev2 :value   [[_ expr]]          (ev2 expr))
(defmethod ev2 :paren   [[_ _ expr _]]      (ev2 expr))
(defmethod ev2 :number  [[_ s]]             (Integer. s))

(def answer2 (->> homework
                  (map (comp ev2 parse2))
                  (reduce +)))
