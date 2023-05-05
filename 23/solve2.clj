(ns advent.2022.23.2
  (:require
    [clojure.string :as s]
    [clojure.set :as cset]
    [util :as u]))

;; Input Handling

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def grid (->> (slurp "input.dat") parse-input))
(def rows (count grid))
(def cols (count (first grid)))
(def init-elves (->> (for [i (range rows) j (range cols)] [i j])
                     (filter #(= \# (get-in grid %)))
                     (into #{})))

;; Logic

(defn calc-nearby-spaces [coords]
  (->> (for [i [-1 0 1] j [-1 0 1]] (u/++ coords [i j]))
       (filter #(not= % coords))
       (into #{})))

(defn no-neighbors? [coords elves]
  (empty? (cset/intersection (calc-nearby-spaces coords) elves)))

(defn free-in-direction? [coords direction elves]
  (let [relevant-spaces (case direction
                          \N (for [i [-1 0 1]] (u/++ coords [-1 i]))
                          \S (for [i [-1 0 1]] (u/++ coords [1 i]))
                          \E (for [i [-1 0 1]] (u/++ coords [i 1]))
                          \W (for [i [-1 0 1]] (u/++ coords [i -1])))]
    (empty? (cset/intersection (set relevant-spaces) elves))))

(defn move [coord direction]
  (case direction
    \N (u/++ coord [-1 0])
    \S (u/++ coord [1 0])
    \W (u/++ coord [0 -1])
    \E (u/++ coord [0 1])))

(defn round [elves directions]
  (let [[d1 d2 d3 d4] (take 4 directions)
        not-needing-to-move (->> elves (filter #(no-neighbors? % elves)) (into #{}))
        needing-to-move (cset/difference elves not-needing-to-move)
        d1-movers (->> needing-to-move (filter #(free-in-direction? % d1 elves)) (into #{}))
        not-d1-movers (cset/difference needing-to-move d1-movers)
        d2-movers (->> not-d1-movers (filter #(free-in-direction? % d2 elves)) (into #{}))
        not-d2-movers (cset/difference not-d1-movers d2-movers)
        d3-movers (->> not-d2-movers (filter #(free-in-direction? % d3 elves)) (into #{}))
        not-d3-movers (cset/difference not-d2-movers d3-movers)
        d4-movers (->> not-d3-movers (filter #(free-in-direction? % d4 elves)) (into #{}))
        cant-move (cset/difference not-d3-movers d4-movers)
        d1-planned-moves (into {} (for [x d1-movers] [x (move x d1)]))
        d2-planned-moves (into {} (for [x d2-movers] [x (move x d2)]))
        d3-planned-moves (into {} (for [x d3-movers] [x (move x d3)]))
        d4-planned-moves (into {} (for [x d4-movers] [x (move x d4)]))
        collided-places (->> (frequencies
                               (concat
                                 (vals d1-planned-moves)
                                 (vals d2-planned-moves)
                                 (vals d3-planned-moves)
                                 (vals d4-planned-moves)))
                             (filter (fn [[loc freq]] (< 1 freq)))
                             (map first)
                             (into #{}))
        not-collided? (fn [[k v]] (not (collided-places v)))
        collided? (fn [[k v]] (collided-places v))
        d1-confirmed-moves (->> d1-planned-moves (filter not-collided?) (into {}))
        d1-cancelled-moves (->> d1-planned-moves (filter collided?) (into {}))
        d2-confirmed-moves (->> d2-planned-moves (filter not-collided?) (into {}))
        d2-cancelled-moves (->> d2-planned-moves (filter collided?) (into {}))
        d3-confirmed-moves (->> d3-planned-moves (filter not-collided?) (into {}))
        d3-cancelled-moves (->> d3-planned-moves (filter collided?) (into {}))
        d4-confirmed-moves (->> d4-planned-moves (filter not-collided?) (into {}))
        d4-cancelled-moves (->> d4-planned-moves (filter collided?) (into {}))]
    (cset/union
      not-needing-to-move
      cant-move
      (set (keys d1-cancelled-moves))
      (set (keys d2-cancelled-moves))
      (set (keys d3-cancelled-moves))
      (set (keys d4-cancelled-moves))
      (set (vals d1-confirmed-moves))
      (set (vals d2-confirmed-moves))
      (set (vals d3-confirmed-moves))
      (set (vals d4-confirmed-moves)))))

(defn count-spaces [elves]
  (let [[r-vals c-vals] [(map first elves) (map second elves)]
        [lowest-r highest-r] [(apply min r-vals) (apply max r-vals)]
        [lowest-c highest-c] [(apply min c-vals) (apply max c-vals)]]
    (-
      (*
        (inc (- highest-r lowest-r))
        (inc (- highest-c lowest-c)))
      (count elves))))

(loop [elves init-elves
       directions (cycle [\N \S \W \E])
       i 0]
  (let [next-elves (round elves directions)]
    (if (= elves next-elves)
      (inc i)
      (recur next-elves (drop 1 directions) (inc i)))))
