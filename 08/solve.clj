(ns advent.2022.8
  (:require
    [clojure.string :as s]
    [clojure.pprint :as pp]))

;; Input Handling

(defn parse-line [input]
  (->> (seq input)
       (map str)
       (map parse-long)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (mapv vec)))

;; Logic

(defn taller-exists? [trees height]
  (empty? (filter #(<= height %) trees)))

(defn solve-1 [input]
  (let [rows (count input)
        cols (count (first input))]
    (frequencies
      (for [r (range rows)
            c (range cols)]
        (let [height (get-in input [r c])
              to-east  (for [r' (range rows) c' (range cols) :when (and (= r r') (< c c'))] (get-in input [r' c']))
              to-west  (for [r' (range rows) c' (range cols) :when (and (= r r') (> c c'))] (get-in input [r' c']))
              to-north (for [r' (range rows) c' (range cols) :when (and (> r r') (= c c'))] (get-in input [r' c']))
              to-south (for [r' (range rows) c' (range cols) :when (and (< r r') (= c c'))] (get-in input [r' c']))]
          (if
            (or
              (taller-exists? to-east height)
              (taller-exists? to-west height)
              (taller-exists? to-south height)
              (taller-exists? to-north height))
            true
            false))))))

(defn view-score [trees height]
  (loop [trees trees
         score 0]
    (if (seq trees)
      (cond
        (< (first trees) height) (recur (rest trees) (inc score))
        (>= (first trees) height) (inc score))
      score)))

(defn solve-2 [input]
  (let [rows (count input)
        cols (count (first input))]
    (apply max
      (for [r (range rows)
            c (range cols)]
        (let [height (get-in input [r c])
              to-east  (for [r' (range rows) c' (range cols) :when (and (= r r') (< c c'))] (get-in input [r' c']))
              to-west  (for [r' (range rows) c' (range cols) :when (and (= r r') (> c c'))] (get-in input [r' c']))
              to-north (for [r' (range rows) c' (range cols) :when (and (> r r') (= c c'))] (get-in input [r' c']))
              to-south (for [r' (range rows) c' (range cols) :when (and (< r r') (= c c'))] (get-in input [r' c']))]
          (*
            (view-score to-east height)
            (view-score (reverse to-west) height)
            (view-score to-south height)
            (view-score (reverse to-north) height)))))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve-2
       pp/pprint))

(solve-file "input.dat")
