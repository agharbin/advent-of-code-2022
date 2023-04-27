(ns advent.2022.21.2
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

(def data (->> (slurp "input.dat")
               parse-input
               (into {})))

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

(def initial-assignments
  (->> data
       (filter (fn [[k v]] (= 1 (count v))))
       (map (fn [[k v]] [k (first v)]))
       (into {})))

(def rules (filter (fn [[k v]] (= 3 (count v))) data))

(defn new-assignments [assignments expr]
  (let [[a [b op c]] expr
        new-expr [(or (assignments a) a) [(or (assignments b) b) op (or (assignments c) c)]]]
    (m/match new-expr
      [(z :guard keyword?) [(y :guard int?) \+ (x :guard int?)]] [z (+ y x)]
      [(z :guard int?) [(y :guard keyword?) \+ (x :guard int?)]] [y (- z x)]
      [(z :guard int?) [(y :guard int?) \+ (x :guard keyword?)]] [x (- z y)]
      [(z :guard keyword?) [(y :guard int?) \- (x :guard int?)]] [z (- y x)]
      [(z :guard int?) [(y :guard keyword?) \- (x :guard int?)]] [y (+ z x)]
      [(z :guard int?) [(y :guard int?) \- (x :guard keyword?)]] [x (- y z)]
      [(z :guard keyword?) [(y :guard int?) \* (x :guard int?)]] [z (* y x)]
      [(z :guard int?) [(y :guard keyword?) \* (x :guard int?)]] [y (/ z x)]
      [(z :guard int?) [(y :guard int?) \* (x :guard keyword?)]] [x (/ z y)]
      [(z :guard keyword?) [(y :guard int?) \/ (x :guard int?)]] [z (/ y x)]
      [(z :guard int?) [(y :guard keyword?) \/ (x :guard int?)]] [y (* z x)]
      [(z :guard int?) [(y :guard int?) \/ (x :guard keyword?)]] [x (/ y z)]
      [z [x op y]] [])))

(def vglr 48165982835110)
(def qhbp 48165982835110)

(loop [assn (-> initial-assignments (dissoc :humn) (assoc :vglr vglr) (assoc :qhbp qhbp))]
  (if (:humn assn)
    (:humn assn)
    (recur
      (into assn (->> (map #(new-assignments assn %) rules)
                      (filter #(not (empty? %))))))))
