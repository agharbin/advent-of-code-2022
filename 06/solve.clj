(ns advent.2022
  (:require
    [clojure.string :as s]))

(def size 14)

(defn parse-input [input]
  (->> input
       s/trim))

(defn all-unique? [xs]
  (= (count xs) (count (set xs))))

(defn solve [input]
  (loop [i 0]
    (if (all-unique? (subs input i (+ i size)))
      (+ i size)
      (recur (inc i)))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
