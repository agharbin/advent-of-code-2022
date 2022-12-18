(ns advent.2022.17.1
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input Handling

(defn parse-input [input]
  (->> input
       s/split-lines
       (apply seq)))

(def input (cycle (parse-input (slurp "input.dat"))))
(def moves (interleave input (cycle [\v])))

;; Logic

(def dash #{[3 2] [3 3] [3 4] [3 5]})
(def cross #{[5 3] [4 2] [4 3] [4 4] [3 3]})
(def l-shape #{[5 4] [4 4] [3 4] [3 3] [3 2]})
(def vertical-line #{[3 2] [4 2] [5 2] [6 2]})
(def square #{[3 2] [3 3] [4 2] [4 3]})
(def shape-seq (cycle [dash cross l-shape vertical-line square]))

(defn highest [world]
  (if (empty? world)
    0
    (->> world
         (map first)
         (apply max))))

(defn shift [shape delta]
  (set (map #(map + % delta) shape)))

(defn collides? [world shape]
  (let [r-vals (map first shape)
        c-vals (map second shape)]
    (not
      (and
        (every? #(<= 0 %) r-vals)
        (every? #(<= 0 % 6) c-vals)
        (empty? (set/intersection world shape))))))

(defn move [shape dir]
  (case dir
    \< (shift shape [0 -1])
    \> (shift shape [0  1])
    \v (shift shape [-1 0])))

(defn place-new-shape [shapes highest-point]
  (let [s (first shapes)
        s' (shift s [(inc highest-point) 0])]
    (conj (rest shapes) s')))

(defn solve [initial-world move-seq shape-seq]
  (loop [world initial-world
         moves moves
         shapes shape-seq
         shapes-seen 0]
    (if (= 2022 shapes-seen)
      (inc (highest world))
      (let [curr-shape (first shapes)
            curr-move (first moves)
            next-pos (move curr-shape curr-move)]
        (cond
          (and (collides? world next-pos) (= \v curr-move))
            (recur
              (set/union world curr-shape)
              (rest moves)
              (place-new-shape (rest shapes) (highest (set/union world curr-shape)))
              (inc shapes-seen))
          (collides? world next-pos)
            (recur
              world
              (rest moves)
              shapes
              shapes-seen)
          :else
            (recur
              world
              (rest moves)
              (conj (rest shapes) next-pos)
              shapes-seen))))))

(solve #{} moves shape-seq)
