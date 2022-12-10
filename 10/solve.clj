(ns advent.2022.10
  (:require
    [clojure.string :as s]
    [clojure.core.match :as m]))

;; Input Handling

(defn parse-line [input]
  (m/match (s/split input #"\s+")
    ["noop"] [:noop]
    ["addx" x] [:addx (parse-long x)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic - Part 1

(defn generate-signal [input]
  (loop [xs input
         register 1
         result []]
    (if (seq xs)
      (m/match (first xs)
        [:noop]   (recur (rest xs) register (conj result register))
        [:addx x] (recur (rest xs) (+ register x) (-> result (conj register) (conj register))))
      result)))

(defn solve-1 [input]
  (let [xs (vec (concat [0] input)) ; correct for 0-indexing
        a (* (xs 20) 20)
        b (* (xs 60) 60)
        c (* (xs 100) 100)
        d (* (xs 140) 140)
        e (* (xs 180) 180)
        f (* (xs 220) 220)]
    (+ a b c d e f)))

;; Logic - Part 2

(defn overlap? [[register position]]
  (<= register position (+ 2 register)))

(defn solve-2 [input]
  (let [xs (mapv vector input (cycle (range 1 41)))]
    (for [x xs] (if (overlap? x) \# \.))))

(defn render [input]
  (->> input
       (partition 40)
       (map #(apply str %))
       (s/join \newline)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       generate-signal
       solve-2
       render
       print))

(solve-file "input.dat")
