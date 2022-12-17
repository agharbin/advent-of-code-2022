(ns advent.2022.16.2
  (:require
    [clojure.string :as s]
    [clojure.set :as set]
    [clojure.math.combinatorics :as combo]
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
(def all-valves (set (keys tunnel-map)))
(defn rate [v] (first (tunnel-map v)))
(def nonzero-valves (set/difference (set (filter #(< 0 (rate %)) all-valves)) #{start}))
(def edges (->> tunnel-map
                (mapcat (fn [[k v]] (for [dst (second v)] [[k dst] 1])))
                (into {})))
(def dists (u/floyd-warshall all-valves edges))

;; Logic

(defn calc-score [time-left v] (* time-left (rate v)))

(defn path-score [path]
  (loop [time-left 26
        score 0
        xs (partition 2 1 path)]
    (if (or (not (seq xs)) (<= time-left 0))
      [score time-left]
      (let [[s d] (first xs)
            time-after-move (dec (- time-left (dists [s d])))]
        (recur time-after-move
               (+ score (calc-score time-after-move d))
               (rest xs))))))

(let [combos (combo/combinations nonzero-valves 7)]
  (prn "total" (count combos))
  (apply max
    (for [c combos]
      (do
        (prn c)
        (let [shortest (apply max (->> (combo/permutations (set c))
                                       (map #(concat [:AA] %))
                                       (map path-score)
                                       (map first)))
              others (set/difference nonzero-valves (set c))
              shortest-2 (apply max (->> (combo/permutations others)
                                         (map #(concat [:AA] %))
                                         (map path-score)
                                         (map first)))]
          (+ shortest shortest-2))))))
