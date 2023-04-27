(ns advent.2022.21.1
  (:require
    [clojure.string :as s]
    [clojure.core.match :as m]))

;; Input Handling

(def re #"(\w+): (.*)")

(defn parse-line [input]
  (let [[_ node-str expr] (re-matches re input)
        node (keyword node-str)
        split-expr (s/split expr #"\s+")]
    (m/match split-expr
      [x]      [node [(parse-long x)]]
      [x op y] [node [(keyword x) (first op) (keyword y)]])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (into {})))

(def data (-> (slurp "input.dat")
              parse-input))

;; Logic

(def solve
  (memoize
    (fn [node]
      (when (not (data node))
        (prn "node" node "not in data"))
      (m/match (data node)
        [x] x
        [x \+ y] (+ (solve x) (solve y))
        [x \- y] (- (solve x) (solve y))
        [x \/ y] (/ (solve x) (solve y))
        [x \* y] (* (solve x) (solve y))))))

(solve :root)
