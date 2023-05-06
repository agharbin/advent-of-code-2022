(ns advent.2022.24
  (:require
    [clojure.string :as s]))

;; Input Handling

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def grid (->> (slurp "input.dat") parse-input))
(def grid-height (count grid))
(def grid-width (count (first grid)))
(def start-pos [0 1])
(def end-pos [(dec grid-height) (- grid-width 2)])

;; Blizzard Logic

(def arrow->dir {\v [1 0] \^ [-1 0] \< [0 -1] \> [0 1]})

(def blizzards
  (->> (for [r (range grid-height) c (range grid-width)] [r c])
       (filter #(#{\> \< \^ \v} (get-in grid %)))
       (map (fn [x] {:loc x :dir (arrow->dir (get-in grid x))}))))

(defn advance-blizzard-loc [{:keys [loc dir]} t]
  (let [inner-height (- grid-height 2)
        inner-width (- grid-width 2)
        inner-loc (mapv + [-1 -1] loc) ; offset to region excluding walls
        [r' c'] (mapv + inner-loc (mapv #(* % t) dir))
        wrapped-r (mod r' inner-height)
        wrapped-c (mod c' inner-width)]
    (mapv + [1 1] [wrapped-r wrapped-c]))) ; offset back to 'real' coordinates

(defn find-blizzard-locs-helper [t]
  (->> blizzards
       (map #(advance-blizzard-loc % t))
       (into #{})))

(def find-blizzard-locs (memoize find-blizzard-locs-helper))

;; Navigation Logic

(def wall-locs
  (->> (for [r (range grid-height) c (range grid-width)] [r c])
       (filter #(#{\#} (get-in grid %)))
       (into #{})))

(def not-wall? (complement wall-locs))
(def all-moves #{[0 0] [1 0] [0 1] [-1 0] [0 -1]})

(defn in-grid? [[r c]]
  (and (<= 0 r)
       (< r grid-height)
       (<= 0 c)
       (< c grid-width)))

(defn possible-next-locs [loc blizzard-locs]
  (->> all-moves ; offsets from current position
       (map #(mapv + loc %)) ; neighbor locations
       (filter not-wall?)
       (filter in-grid?)
       (filter (complement blizzard-locs))))

(defn bfs [start-loc goal-loc start-time]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start-time start-loc])
         seen-states #{}]
    (when (seq q)
      (let [[time-passed loc :as state] (peek q)]
        (cond
          (= loc goal-loc) time-passed
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (possible-next-locs loc (find-blizzard-locs (inc time-passed)))]
              (recur
                (into (pop q) (for [c candidates] [(inc time-passed) c]))
                (conj seen-states state))))))))

;; Solutions

(defn solve-1 []
  (bfs start-pos end-pos 0))

(defn solve-2 []
  (let [time-after-first-leg (bfs start-pos end-pos 0)
        time-after-second-leg (bfs end-pos start-pos time-after-first-leg)
        time-after-third-leg (bfs start-pos end-pos time-after-second-leg)]
    time-after-third-leg))

(solve-1)
