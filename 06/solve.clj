(ns advent.2022.06
  (:require
    [clojure.string :as s]))

(def size 14)

(defn parse-input [input]
  (->> input
       s/trim))

(defn solve [input]
  (loop [i 0]
    (if (apply distinct? (subs input i (+ i size)))
      (+ i size)
      (recur (inc i)))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
