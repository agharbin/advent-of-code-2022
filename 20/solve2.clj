(ns advent.2022
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

(def key 811589153)

(defn parse-line [input]
  (* (parse-long input) key))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (map vector (range))
       (map (fn [[k v]] {:id k :value v}))))

(defn calc-next-map [input]
  (->
    (into {} (map vector input (drop 1 input)))
    (assoc (last input) (first input))))

(defn move-forward [next-map prev-map x]
  (let [init-n (next-map x)
        init-p (prev-map x)
        next-map' (assoc next-map init-p init-n)
        prev-map' (assoc prev-map init-n init-p)
        [new-p new-n] (->> (iterate next-map' init-n)
                           (take (+ 1 (mod (:value x) (dec (count next-map))))) ; one for 'init' value
                           (take-last 2))]
    [(-> next-map' (assoc new-p x) (assoc x new-n))
     (-> prev-map' (assoc x new-p) (assoc new-n x))]))

(defn move-backward [next-map prev-map x]
  (let [init-n (next-map x)
        init-p (prev-map x)
        next-map' (assoc next-map init-p init-n)
        prev-map' (assoc prev-map init-n init-p)
        [new-n new-p] (->> (iterate prev-map' init-p)
                           (take (+ 1 (mod (abs (:value x)) (dec (count next-map))))) ; one for 'init' value
                           (take-last 2))]
    [(-> next-map' (assoc new-p x) (assoc x new-n))
     (-> prev-map' (assoc x new-p) (assoc new-n x))]))

(defn calc-result [next-map]
  (let [zero-val (first (filter #(zero? (:value %)) (keys next-map)))
        result-seq (iterate next-map zero-val)]
    (+
     (:value (nth result-seq 1000))
     (:value (nth result-seq 2000))
     (:value (nth result-seq 3000)))))

(defn move [next-map prev-map x]
  (let [v (:value x)]
    (cond
      (zero? v) [next-map prev-map]
      (pos? v)  (move-forward next-map prev-map x)
      (neg? v)  (move-backward next-map prev-map x)
      :else "error")))

(defn solve [input]
  (let [init-next-map (calc-next-map input)
        init-prev-map (set/map-invert init-next-map)]
    (loop [xs (apply concat (repeat 10 input))
           next-map init-next-map
           prev-map init-prev-map]
      (if (seq xs)
        (let [[nm pm] (move next-map prev-map (first xs))]
          (recur (rest xs) nm pm))
        (calc-result next-map)))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve))

(solve-file "input.dat")
