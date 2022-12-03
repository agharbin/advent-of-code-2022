(ns advent.2022.03.2
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn get-value [c]
  (if (<= (int \a) (int c) (int \z))
    (inc (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(defn common-item [[elf-1 elf-2 elf-3]]
    (->> (set/intersection (set elf-1) (set elf-2) (set elf-3))
         first
         get-value))

(defn solve [input]
  (apply + (map common-item input)))

(defn parse-input [input]
  (->> (s/split-lines input)
       (partition 3)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       print))

(solve-file "input.dat")
