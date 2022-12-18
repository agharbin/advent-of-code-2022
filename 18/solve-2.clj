(ns advent.2022
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input Handling

(defn parse-line [input]
  (let [[_ a b c] (re-matches #"(\d+),(\d+),(\d+)" input)]
    (mapv parse-long [a b c])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn all-facings [[a b c]]
  (let [a' (inc a)
        b' (inc b)
        c' (inc c)]
    #{#{[a b c] [a b' c] [a b c'] [a b' c']}
      #{[a' b c] [a' b' c] [a' b c'] [a' b' c']}
      #{[a b c] [a' b c] [a b' c] [a' b' c]}
      #{[a' b' c'] [a' b c'] [a b' c'] [a b c']}
      #{[a b c] [a' b c] [a b c'] [a' b c']}
      #{[a b' c] [a' b' c] [a b' c'] [a' b' c']}}))

(def bound 25)

(defn next-blocks [[a b c]]
  (for [[a' b' c'] #{[(inc a) b c] [(dec a) b c] [a (inc b) c] [a (dec b) c] [a b (inc c)] [a b (dec c)]}
        :when (and (<= -1 a' bound) (<= -1 b' bound) (<= -1 c' bound))]
    [a' b' c']))

(defn solve [input]
  (let [input-set (set input)
        facings (set (mapcat all-facings input))]
    (loop [q [[-1 -1 -1]]
           facings-reached #{}
           visited #{}]
      (if (seq q)
        (if (or (visited (first q)) (input-set (first q)))
          (recur (rest q) facings-reached visited)
          (let [block (first q)
                touched-facings (set/intersection (all-facings block) facings)
                next-b (next-blocks block)]
            (recur (into (rest q) next-b) (set/union facings-reached touched-facings) (conj visited block))))
        (count facings-reached)))))

(defn solve-file [input]
  (->> input
       parse-input
       solve
       prn))

(solve-file (slurp "input.dat"))
