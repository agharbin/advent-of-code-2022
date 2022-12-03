(ns advent.2022.03.1
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn get-value [c]
  (if (<= (int \a) (int c) (int \z))
    (inc (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(defn common-item [input]
  (let [len (/ (count input) 2)
        comp-1 (set (subs input 0 len))
        comp-2 (set (subs input len))]
    (->> (set/intersection comp-1 comp-2)
         first
         get-value)))

(defn solve [input]
  (apply + (map common-item input)))

(defn parse-input [input]
  (s/split-lines input))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       print))

(solve-file "input.dat")
