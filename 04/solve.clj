(ns advent.2022.04
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def re #"(\d+)-(\d+),(\d+)-(\d+)")

(defn parse-line [input]
  (let [[_ a b c d] (re-matches re input)]
    [(parse-long a) (parse-long b) (parse-long c) (parse-long d)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(defn overlaps? [[a b c d]]
  (not
    (empty?
      (set/intersection
        (set (range a (inc b)))
        (set (range c (inc d)))))))

(defn solve [input]
  (frequencies (map overlaps? input)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
