(ns advent.2022.01
  (:require [clojure.string :as s]))

(defn solve [input]
  (->> input
       (map #(apply + %))
       sort
       reverse
       (take 3)
       (apply +)))

(defn parse-elf [input]
  (->> input
       s/split-lines
       (map parse-long)))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (map parse-elf)))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       prn))

(solve-file "input.dat")
