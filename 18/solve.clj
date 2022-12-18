(ns advent.2022
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input Handling

(defn parse-line [input]
  (let [[_ a b c] (re-matches #"(\d+),(\d+),(\d+)" input)]
    (mapv parse-long [a b c])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn all-facings [[a b c]]
  (let [a' (inc a)
        b' (inc b)
        c' (inc c)]
    #{#{[a b c] [a b' c] [a b c'] [a b' c']}
      #{[a' b c] [a' b' c] [a' b c'] [a' b' c']}
      #{[a b c] [a' b c] [a b' c] [a' b' c]}
      #{[a' b' c'] [a' b c'] [a b' c'] [a b c']}
      #{[a b c] [a' b c] [a b c'] [a' b c']}
      #{[a b' c] [a' b' c] [a b' c'] [a' b' c']}}))

(defn solve [input]
  (->> input
       (mapcat all-facings)
       frequencies
       (filter #(= 1 (second %)))
       (map first)
       count))

(defn solve-file [input]
  (->> input
       parse-input
       solve
       prn))

(solve-file (slurp "input.dat"))
