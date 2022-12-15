(ns advent.2022
  (:require
    [clojure.string :as s]))

;; Input Handling

(def re #"(\d+),(\d+)")

(defn parse-line [input]
  (->> (re-seq re input)
       (map rest)
       (map #(map parse-long %))))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn points-between [[[x1 y1] [x2 y2]]]
  (let [min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))]
     [x y])))

(defn covered-points [segments]
  (->> (partition 2 1 segments)
       (mapcat points-between)
       (into #{})))

(defn place-sand [rocks y-extent [x y]]
  (cond
    (< y-extent y)                  rocks
    (not (rocks [x (inc y)]))       (recur rocks y-extent [x (inc y)])
    (not (rocks [(dec x) (inc y)])) (recur rocks y-extent [(dec x) (inc y)])
    (not (rocks [(inc x) (inc y)])) (recur rocks y-extent [(inc x) (inc y)])
    :else (conj rocks [x y])))

(defn solve [input]
  (let [rocks (->> input (mapcat covered-points) (into #{}))
        y-extent (apply max (map second rocks))]
    (loop [r rocks
           i 0]
      (let [r' (place-sand r y-extent [500 0])]
        (if (= (count r') (count r))
          i
          (recur r' (inc i)))))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
