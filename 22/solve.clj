(ns advent.2022.22.1
  (:require
    [clojure.set :as cset]
    [clojure.string :as s]
    [util :as u]))

;; Input Handling

(defn parse-instruction [xs]
  (if (#{\R \L} (first xs))
    (first xs)
    (parse-long (apply str xs))))

(defn pad-to [line length]
  (if (< (count line) length)
    (let [df (- length (count line))]
      (vec (concat line (repeat df \space))))
    line))

(defn parse-input [input]
  (let [[grid-str moves-str] (s/split input #"\n\n")
        grid (->> grid-str s/split-lines (mapv vec))
        max-length (apply max (map count grid))
        grid (mapv #(pad-to % max-length) grid)
        moves (->> moves-str s/trim (partition-by #{\R \L}) (map parse-instruction))]
    [grid moves]))

;; Logic

(def right
  {[0 1]  [1 0]
   [1 0]  [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]})

(def left (cset/map-invert right))

(defn wrap [grid [r c]]
  [(mod r (count grid)) (mod c (count (first grid)))])

(defn find-next-coords [grid loc dir]
  (let [proposed-loc (wrap grid (map + loc dir))]
    (if (= \space (get-in grid proposed-loc))
      (recur grid proposed-loc dir)
      proposed-loc)))

(defn move [grid loc dir]
  (let [proposed-loc (find-next-coords grid loc dir)
        grid-contents (get-in grid proposed-loc)]
    (case grid-contents
      \# loc
      \. proposed-loc)))

(defn score [[r c] dir]
  (let [facing-score ({[0 1] 0 [1 0] 1 [0 -1] 2 [-1 0] 3} dir)]
    (+ (* 1000 (inc r))
       (* 4    (inc c))
       facing-score)))

(defn solve [[grid instructions]]
  (let [start-loc [0 (u/index-find (first grid) \.)]
        start-dir [0 1]]
    (loop [xs instructions
           loc start-loc
           dir start-dir]
      (if (seq xs)
        (let [x (first xs)]
          (case x
            \R (recur (rest xs) loc (right dir))
            \L (recur (rest xs) loc (left dir))
            (recur
              (if (= x 1) (rest xs) (conj (rest xs) (dec x)))
              (move grid loc dir)
              dir)))
        (score loc dir)))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
