(ns advent.2022.13.1
  (:require
    [clojure.string :as s]
    [util :as u]))

(defn parse-pair [input]
  (let [[l1 l2]  (s/split-lines input)]
    (mapv read-string [l1 l2])))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (map parse-pair)))

(defn signal-comp [x y]
  (cond
    (and (number? x) (number? y))  (compare x y)
    (and (number? x) (vector? y))  (signal-comp [x] y)
    (and (vector? x) (number? y))  (signal-comp x [y])
    (and (vector? x) (vector? y))
      (cond (and (empty? x) (not (empty? y))) -1
            (and (not (empty? x)) (empty? y)) 1
            (and (empty? x) (empty? y)) 0
            :else (let [result (signal-comp (first x) (first y))]
                    (if (zero? result)
                      (signal-comp (vec (rest x)) (vec (rest y)))
                      result)))))

(defn solve [input]
  (map (fn [[x y]] (< (signal-comp x y) 0))
       input))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       (u/zip (range 1 500))
       (filter #(true? (second %)))
       (map first)
       (apply + )
       prn))

(solve-file "input.dat")
