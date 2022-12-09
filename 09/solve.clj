(ns advent.2022
  (:require
    [clojure.string :as s]
    [clojure.pprint :as pp]))

;; Input Handling

(defn parse-line [input]
  (let [[d n] (s/split input #" ")]
    [(first d) (parse-long n)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn dist [h t]
  (apply + (map abs (map - h t))))

(defn follow [[hx hy :as head] [tx ty :as tail]]
  "If we are more than 1 space away, find the move for tail that results in the minimal distance to head"
  (if (and (< (abs (- hx tx)) 2) (< (abs (- hy ty)) 2))
    tail
    (reduce #(if (< (dist head %1) (dist head %2)) %1 %2)
            (for [dx [-1 0 1] dy [-1 0 1]] (mapv + tail [dx dy])))))

(defn move [[hx hy] d]
  (case d
    \L [(dec hx) hy]
    \R [(inc hx) hy]
    \U [hx (inc hy)]
    \D [hx (dec hy)]))

(defn solve-1 [input]
  (loop [input input
         head [0 0]
         tail head
         touched #{tail}]
    (if (seq input)
      (let [[d n] (first input)
            new-head (move head d)
            new-tail (follow new-head tail)]
        (recur
          (if (= n 1) (rest input) (conj (rest input) [d (dec n)]))
          new-head
          new-tail
          (conj touched new-tail)))
      (count touched))))

(defn solve-2 [input]
  (loop [input (seq input)
         head [0 0]
         tail-1 head
         tail-2 head
         tail-3 head
         tail-4 head
         tail-5 head
         tail-6 head
         tail-7 head
         tail-8 head
         tail-9 head
         touched #{tail-9}]
    (if (seq input)
      (let [[d n] (first input)
            new-head (move head d)
            new-tail-1 (follow new-head tail-1)
            new-tail-2 (follow new-tail-1 tail-2)
            new-tail-3 (follow new-tail-2 tail-3)
            new-tail-4 (follow new-tail-3 tail-4)
            new-tail-5 (follow new-tail-4 tail-5)
            new-tail-6 (follow new-tail-5 tail-6)
            new-tail-7 (follow new-tail-6 tail-7)
            new-tail-8 (follow new-tail-7 tail-8)
            new-tail-9 (follow new-tail-8 tail-9)]
        (recur
          (if (= n 1) (rest input) (conj (rest input) [d (dec n)]))
          new-head
          new-tail-1
          new-tail-2
          new-tail-3
          new-tail-4
          new-tail-5
          new-tail-6
          new-tail-7
          new-tail-8
          new-tail-9
          (conj touched new-tail-9)))
      (count touched))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve-2
       pp/pprint))

(solve-file "input.dat")
