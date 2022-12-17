(ns advent.2022.15.1
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input Handling

(def regex #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn parse-line [input]
  (let [[_ a b c d] (re-matches regex input)]
    [[(parse-long a) (parse-long b)] [(parse-long c) (parse-long d)]]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def row 2000000)
(def max-dist 1936605)

(defn x-vals [[[x1 y1] [x2 y2]]] [x1 x2])
(defn y-vals [[[x1 y1] [x2 y2]]] [y1 y2])
(defn beacons [[[x1 y1] [x2 y2]]] [x2 y2])

(defn dist [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn solve [input]
  (let [min-x (apply min (mapcat x-vals input))
        max-x (apply max (mapcat x-vals input))
        min-y (apply min (mapcat y-vals input))
        max-y (apply max (mapcat y-vals input))
        covered-spaces (set
                        (filter some?
                          (for [i (range (- min-x max-dist) (+ max-x max-dist))
                                [[sx sy] [bx by]] input]
                            (if (<= (dist [sx sy] [i row]) (dist [sx sy] [bx by]))
                              i
                              nil))))
        occupied-spaces (->> input (map beacons) set (filter #(= (second %) row)) (map first))
        off-limits (set/difference covered-spaces occupied-spaces)]
    (count off-limits)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
