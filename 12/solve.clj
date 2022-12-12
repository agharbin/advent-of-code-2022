(ns advent.2022.12
  (:require
    [clojure.string :as s]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map vec)
       vec))

(def grid (->> (slurp "input.dat") parse-input))

(defn search [x]
  (for [r (range (count grid))
        c (range (count (first grid)))
        :when (= x (get-in grid [r c]))]
      [r c]))

(def S (first (search \S)))
(def E (first (search \E)))

(defn goal? [s]
  (= s E))

(defn height [c]
  (case c
    \S (int \a)
    \E (int \z)
    (int c)))

(defn possible-next-states [[r c]]
  (let [candidates (for [[dr dc] [[0 1] [0 -1] [1 0] [-1 0]]
                         :when (get-in grid [(+ r dr) (+ c dc)])]
                     [(+ r dr) (+ c dc)])
        curr-height (height (get-in grid [r c]))
        legit-candidates (filter #(<= (- (height (get-in grid %)) curr-height) 1) candidates)]
    legit-candidates))

(defn bfs [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (if (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal? state) dist
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (possible-next-states state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state)))))
      Integer/MAX_VALUE)))

(apply min
  (for [start (search \a)]
    (bfs start)))
