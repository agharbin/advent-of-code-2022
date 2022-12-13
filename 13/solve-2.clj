(ns advent.2022.13.2
  (:require
    [clojure.string :as s]
    [util :as u]))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (mapcat s/split-lines)
       (map read-string)))

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

(defn solve-file [input]
  (let [processed  (->> input parse-input (concat [[[2]] [[6]]]) (sort signal-comp) vec)]
    (*
      (inc (u/index-find processed [[2]]))
      (inc (u/index-find processed [[6]])))))

(solve-file (slurp "input.dat"))
