(ns day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]))

(defn parse-line
  [line]
  (->> (str/split line #"\s")
       (map #(str/split % #":"))
       (into {})
       walk/keywordize-keys))

(def passports
  (with-open [rdr (io/reader "./day4.input")]
    (loop [[hd & tl] (line-seq rdr)
           acc {}
           passports []]
      (if hd
        (if (str/blank? hd)
          (recur tl {} (conj passports acc))
          (recur tl (merge acc (parse-line hd)) passports))
        (if (not (empty? acc))
          (conj passports acc)
          passports)))))

(s/def ::byr string?)
(s/def ::iyr string?)
(s/def ::eyr string?)
(s/def ::hgt string?)
(s/def ::hcl string?)
(s/def ::ecl string?)
(s/def ::pid string?)
(s/def ::cid string?)

(s/def ::passport
  (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
          :opt-un [::cid]))

(def answer1 (count (filter #(s/valid? ::passport %) passports)))

;; Some helpers to write the new specs

(defn number
  "The number represented by the string, or nil."
  [s]
  (when (string? s)
    (when-let [n (edn/read-string s)]
      (when (number? n)
        n))))

(s/def ::number (s/conformer #(or (number %) ::s/invalid)))

(defn unit
  "A spec for a number followed by the given unit."
  [unit]
  (let [pattern (re-pattern (str "^([0-9]+)" unit))]
    (s/conformer (fn [s]
                   (or (when (string? s)
                         (when-let [[_ mag] (re-matches pattern s)]
                           (number mag)))
                       ::s/invalid)))))

(s/def ::byr (s/and ::number #(<= 1920 % 2002)))
(s/def ::iyr (s/and ::number #(<= 2010 % 2020)))
(s/def ::eyr (s/and ::number #(<= 2020 % 2030)))
(s/def ::hgt (s/or :cm (s/and (unit "cm")
                              #(<= 150 % 193))
                   :in (s/and (unit "in")
                              #(<= 59 % 76))))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}" %))

(def answer2 (count (filter #(s/valid? ::passport %) passports)))
