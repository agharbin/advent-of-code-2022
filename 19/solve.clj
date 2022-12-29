(ns advent.2022.19
  (:require
    [clojure.string :as s]))

;; Input Handling

(def re #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.*")

(defn parse-line [input]
  (let [[_ b r1 c1 o1 o2 g1 g2] (re-matches re input)]
    [(parse-long b) {:ore {:ore (parse-long r1)}
                     :clay {:ore (parse-long c1)}
                     :obsidian {:ore (parse-long o1) :clay (parse-long o2)}
                     :geode {:ore (parse-long g1) :obsidian (parse-long g2)}}]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (take 3)
       (into {})))

;; Logic

(def minutes 32)
(def max-ore 4)
(def max-clay 14)
(def max-obsidian 16)

(defn available-choices [ore clay obsidian ore-bots clay-bots obsidian-bots blueprint]
  (cond-> (list)
    (and (pos? ore-bots) (< ore-bots max-ore))
      (conj :ore)
    (and (pos? ore-bots) (< clay-bots max-clay))
      (conj :clay)
    (and (pos? ore-bots)
         (pos? clay-bots)
         (< obsidian-bots max-obsidian))
      (conj :obsidian)
    (and (pos? ore-bots)
         (pos? obsidian-bots))
      (conj :geode)))

(defn next-state [bot-to-build
                  ore clay obsidian geodes
                  ore-bots clay-bots obsidian-bots geode-bots
                  ts blueprint]
  (let [ore-cost (get-in blueprint [bot-to-build :ore] 0)
        clay-cost (get-in blueprint [bot-to-build :clay] 0)
        obsidian-cost (get-in blueprint [bot-to-build :obsidian] 0)]
    (cond
      (= ts minutes)
        [ore clay obsidian geodes ore-bots clay-bots obsidian-bots geode-bots ts blueprint]
      (and (<= ore-cost ore) (<= clay-cost clay) (<= obsidian-cost obsidian))
        [(+ (- ore ore-cost) ore-bots)
         (+ (- clay clay-cost) clay-bots)
         (+ (- obsidian obsidian-cost) obsidian-bots)
         (+ geodes geode-bots)
         (if (= :ore bot-to-build) (inc ore-bots) ore-bots)
         (if (= :clay bot-to-build) (inc clay-bots) clay-bots)
         (if (= :obsidian bot-to-build) (inc obsidian-bots) obsidian-bots)
         (if (= :geode bot-to-build) (inc geode-bots) geode-bots)
         (inc ts)
         blueprint]
      :else
        (recur
          bot-to-build
          (+ ore ore-bots)
          (+ clay clay-bots)
          (+ obsidian obsidian-bots)
          (+ geodes geode-bots)
          ore-bots
          clay-bots
          obsidian-bots
          geode-bots
          (inc ts)
          blueprint))))

(def best-result
  (fn [ore clay obsidian geodes
       ore-bots clay-bots obsidian-bots geode-bots
       ts blueprint]
    (cond
      (= minutes ts)
        [geodes [ore clay obsidian geodes] [ore-bots clay-bots obsidian-bots geode-bots]]
      :else
        (let [choices (available-choices ore clay obsidian ore-bots clay-bots obsidian-bots blueprint)]
          (reduce (fn [x y] (if (< (first x) (first y)) y x))
            (map #(apply best-result (next-state %
                                                 ore clay obsidian geodes
                                                 ore-bots clay-bots obsidian-bots geode-bots
                                                 ts blueprint))
                 choices))))))

;; Solution

(def input (->> (slurp "input.dat") parse-input))

(prn
  (*
    (first (best-result 0 0 0 0 1 0 0 0 0 (input 1)))
    (first (best-result 0 0 0 0 1 0 0 0 0 (input 2)))
    (first (best-result 0 0 0 0 1 0 0 0 0 (input 3)))))
