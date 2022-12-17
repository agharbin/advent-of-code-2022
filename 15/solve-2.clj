(ns advent.2022.15.2
  (:require
    [clojure.string :as s]))

;; Handle Input

(def regex #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn parse-line [input]
  (let [[_ a b c d] (re-matches regex input)]
    [[(parse-long a) (parse-long b)] [(parse-long c) (parse-long d)]]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn dist [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def min-val 0)
(def max-val 4000000)

(defn ring [[[x1 y1] [x2 y2]]]
  (let [d (inc (dist [x1 y1] [x2 y2]))]
    (loop [xs [[x2 y2]]
           points #{}]
      (if (seq xs)
        (let [[x y] (first xs)
              nearby (for [i [-1 0 1] j [-1 0 1]
                           :when (and
                                   (= d (dist [x1 y1] [(+ x i) (+ y j)]))
                                   (not (points [(+ x i) (+ y j)])))]
                       [(+ x i) (+ y j)])]
          (recur
            (into (rest xs) nearby)
            (into points nearby)))
        points))))

(defn outside-range? [p [s b]]
  (< (dist s b) (dist s p)))

(defn outside-all? [p pairs]
  (every? true? (for [pair pairs] (outside-range? p pair))))

(defn solve [input]
  (loop [xs input]
    (if (seq xs)
      (let [candidate (first xs)
            ring-points (->> (ring candidate) (filter
                                                (fn [[x y]]
                                                  (and
                                                    (<= min-val x max-val)
                                                    (<= min-val y max-val)))))
            valid-points (filter #(outside-all? % input) ring-points)]
        (if (not (empty? valid-points))
          (prn valid-points)
          (recur (rest xs))))
      "Not found")))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
