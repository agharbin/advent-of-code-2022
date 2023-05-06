(ns advent.2022.25
  (:require
    [clojure.string :as s]
    [clojure.set :as cset]))

(def input (->> (slurp "input.dat") s/split-lines (mapv vec)))

(def symbol->value {\2 2 \1 1 \0 0 \- -1 \= -2})
(def value->symbol (cset/map-invert symbol->value))

(defn normalize [xs]
  (map symbol->value (reverse xs)))

(defn de-normalize [xs]
  (map value->symbol (reverse xs)))

(defn normalized-snafu-add [x-seq y-seq]
  (loop [xs x-seq
         ys y-seq
         result-seq []
         carry 0]
    (cond
      (and (seq xs) (seq ys))
        (let [x (first xs)
              y (first ys)
              result (+ x y carry)]
          (cond
            (< result -2) (recur (rest xs) (rest ys) (conj result-seq (+ result 5)) -1)
            (< 2 result)  (recur (rest xs) (rest ys) (conj result-seq (- result 5))  1)
            :else         (recur (rest xs) (rest ys) (conj result-seq result)        0)))
      (seq xs)
        (recur xs '(0) result-seq carry)
      (seq ys)
        (recur '(0) ys result-seq carry)
      (not= 0 carry)
        (recur '(0) '(0) result-seq carry)
      :else result-seq)))

(defn snafu+ [x y]
  (de-normalize (normalized-snafu-add (normalize x) (normalize y))))

(->> (reduce snafu+ input) (apply str))
