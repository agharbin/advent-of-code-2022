(ns advent.2022.7
  (:require
    [clojure.string :as s]
    [clojure.core.match :as m]
    [clojure.pprint :as pp]))

;; Input Handling

(defn parse-line [input]
  (m/match (s/split input #"\s+")
    ["$" "cd" ".."] [:cd]
    ["$" "cd" x]    [:cd x]
    ["$" "ls"]      [:ls]
    ["dir" x]       [:dir x]
    [size x]        [:file (parse-long size) x]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn construct-filesystem [input]
  (loop [inputs (seq input)
         tree {}
         path []
         seen-dirs []]
    (if (seq inputs)
      (m/match (first inputs)
        [:cd]          (recur (rest inputs) tree (pop path) seen-dirs)
        [:cd x]        (recur (rest inputs) tree (conj path x) seen-dirs)
        [:ls]          (recur (rest inputs) tree path seen-dirs)
        [:dir x]       (recur (rest inputs) (assoc-in tree (concat path [x]) {}) path (conj seen-dirs (concat path [x])))
        [:file size x] (recur (rest inputs) (assoc-in tree (concat path [x]) size) path seen-dirs))
      [tree seen-dirs])))

(defn size-of-directory [tree path]
  (->> (get-in tree path)
       (tree-seq map? vals)
       (filter number?)
       (apply +)))

(defn solve-1 [input]
  (let [[tree dirs] (construct-filesystem input)]
    (->> (for [d dirs] (size-of-directory d))
         (filter #(<= % 100000))
         (apply +))))

(defn solve-2 [input]
  (let [[tree dirs] (construct-tree input)
        size-to-free (- 30000000 (- 70000000 (size-of-directory tree ["/"])))]
    (->> (for [d dirs] (size-of-directory tree d))
         (filter  #(>= % size-to-free))
         (apply min))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve-2
       pp/pprint))

(solve-file "input.dat")
