(ns advent.2022.5
  (:require
    [clojure.string :as s]
    [clojure.pprint :as pp]))

;; Hand-copying the stacks portion of the input since it is faster than writing parsing logic

(def stacks
  {1 [\B \Z \T]
   2 [\V \H \T \D \N]
   3 [\B \F \M \D]
   4 [\T \J \G \W \V \Q \L]
   5 [\W \D \G \P \V \F \Q \M]
   6 [\V \Z \Q \G \H \F \S]
   7 [\Z \S \N \R \L \T \C \W]
   8 [\Z \H \W \D \J \N \R \M]
   9 [\M \Q \L \F \D \S]})

;; Input Parsing

(def re #"move (\d+) from (\d+) to (\d+).*")

(defn parse-instruction [input]
  (let [[_ n source dest] (re-matches re input)]
    (mapv parse-long [n source dest])))

(defn parse-instructions [input]
  (->> input
       s/split-lines
       (map parse-instruction)))

(defn parse-input [input]
  (let [[_ instructions-str] (s/split input #"\n\n")]
    (parse-instructions instructions-str)))

;; Logic

(defn move-1 [stack [n source dest]]
  (if (zero? n)
    stack
    (move
      (-> stack
          (update-in [source] pop)
          (update-in [dest] #(conj % (last (get-in stack [source])))))
      [(dec n) source dest])))

(defn move-2 [stack [n source dest]]
  (let [source-size (count (stack source))
        new-size (- source-size n)
        boxes-to-move (drop new-size (stack source))]
    (-> stack
        (update-in [source] #(take new-size %))
        (update-in [dest] #(concat % boxes-to-move)))))

(defn solve [input]
  (reduce move-2 stacks input))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       sort
       (map second)
       (map last)
       (apply str)
       pp/pprint))

(solve-file "input.dat")
