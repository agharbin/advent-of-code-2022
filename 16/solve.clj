(ns advent.2022.16.1
  (:require
    [clojure.string :as s]
    [clojure.set :as set]
    [util :as u]))

;; Input Handling

(def re #"Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)")

(defn parse-line [input]
  (let [[_ valve rate connection-string] (re-matches re input)
        connections (s/split connection-string #", ")]
    [(keyword valve) [(parse-long rate) (set (map keyword connections))]]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (into {})))

;; Data

(def start :AA)
(def tunnel-map (->> (slurp "input.dat") parse-input))
(defn rate [v] (first (tunnel-map v)))
(def all-valves (set (keys tunnel-map)))
(def nonzero-valves (set/difference (set (filter #(< 0 (rate %)) all-valves)) #{start}))
(def edges (->> tunnel-map
                (mapcat (fn [[k v]] (for [dst (second v)] [[k dst] 1])))
                (into {})))
(def dists (u/floyd-warshall all-valves edges))

;; Logic

(defn calc-score [time-left v] (* time-left (rate v)))

(defn path-score [path]
  (loop [time-left 30
        score 0
        xs (partition 2 1 path)]
    (if (or (not (seq xs)) (<= time-left 0))
      [score time-left]
      (let [[s d] (first xs)
            time-after-move (dec (- time-left (dists [s d])))]
        (recur time-after-move
               (+ score (calc-score time-after-move d))
               (rest xs))))))

(defn score-order [x y]
  (let [[sx _] (path-score x)
        [sy _] (path-score y)]
    (- (compare sx sy))))

(defn solve [path]
  (let [loc (last path)
        [score time-left] (path-score path)]
    (if (or (<= time-left 0) (= (set path) all-valves))
      [path score]
      (let [possible-next-paths (map #(concat path [%]) (set/difference nonzero-valves (set path)))
            sorted-paths (sort score-order possible-next-paths)]
        (->> sorted-paths (map solve) (reduce #(if (> (second %1) (second %2)) %1 %2)))))))

(solve [:AA])
