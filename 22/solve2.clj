(ns advent.2022.22.2
  (:require
    [clojure.set :as cset]
    [clojure.string :as s]
    [util :as u]))

;; Input Handling

(defn parse-instruction [xs]
  (if (#{\R \L} (first xs))
    (first xs)
    (parse-long (apply str xs))))

(defn pad-to [line length]
  (if (< (count line) length)
    (let [df (- length (count line))]
      (vec (concat line (repeat df \space))))
    line))

(defn parse-input [input]
  (let [[grid-str moves-str] (s/split input #"\n\n")
        grid (->> grid-str s/split-lines (mapv vec))
        max-length (apply max (map count grid))
        grid (mapv #(pad-to % max-length) grid)
        moves (->> moves-str s/trim (partition-by #{\R \L}) (map parse-instruction))]
    [grid moves]))

;; Logic

(def grid-right
  {[0 1]  [1 0]
   [1 0]  [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]})

(def grid-left (cset/map-invert grid-right))

(defn grid-forward [loc heading]
  (u/++ loc heading))

(def cube-len 50)
(def full-input (parse-input (slurp "input.dat")))
(def grid (first full-input))
(def grid-height (count grid))
(def grid-width (count (first grid)))
(def instructions (second full-input))

(defn transpose [m]
  (apply map vector m))

(defn matrix-multiply [m v]
  (->> (transpose m)
       (mapv #(mapv * v %))
       (mapv #(apply + %))))

(defn x-rotate-left [v]
  (matrix-multiply
    [[ 1  0  0]
     [ 0  0 -1]
     [ 0  1  0]]
    v))

(defn x-rotate-right [v]
  (matrix-multiply
    [[ 1  0  0]
     [ 0  0  1]
     [ 0 -1  0]]
    v))

(defn y-rotate-left [v]
  (matrix-multiply
    [[ 0  0  1]
     [ 0  1  0]
     [-1  0  0]]
    v))

(defn y-rotate-right [v]
  (matrix-multiply
    [[ 0  0 -1]
     [ 0  1  0]
     [ 1  0  0]]
    v))

(defn z-rotate-left [v]
  (matrix-multiply
    [[ 0 -1  0]
     [ 1  0  0]
     [ 0  0  1]]
    v))

(defn z-rotate-right [v]
  (matrix-multiply
    [[ 0  1  0]
     [-1  0  0]
     [ 0  0  1]]
    v))

(defn spin-right [heading normal]
  (case normal
    [ 1  0  0] (x-rotate-right heading)
    [-1  0  0] (x-rotate-left heading)
    [ 0  1  0] (y-rotate-right heading)
    [ 0 -1  0] (y-rotate-left heading)
    [ 0  0  1] (z-rotate-right heading)
    [ 0  0 -1] (z-rotate-left heading)))

(defn spin-left [heading normal]
  (case normal
    [ 1  0  0] (x-rotate-left heading)
    [-1  0  0] (x-rotate-right heading)
    [ 0  1  0] (y-rotate-left heading)
    [ 0 -1  0] (y-rotate-right heading)
    [ 0  0  1] (z-rotate-left heading)
    [ 0  0 -1] (z-rotate-right heading)))

(defn cross-product [[a1 a2 a3] [b1 b2 b3]]
  [(- (* a2 b3) (* a3 b2))
   (- (* a3 b1) (* a1 b3))
   (- (* a1 b2) (* a2 b1))])

(defn march [loc normal heading]
  (let [[x' y' z' :as loc'] (u/++ loc heading)
        cross (cross-product heading normal)
        rotation (case cross
                   [ 1  0  0] x-rotate-left
                   [-1  0  0] x-rotate-right
                   [ 0  1  0] y-rotate-left
                   [ 0 -1  0] y-rotate-right
                   [ 0  0  1] z-rotate-left
                   [ 0  0 -1] z-rotate-right)]
    (cond
      (or (< x' 0) (< y' 0) (< z' 0) (= x' cube-len) (= y' cube-len) (= z' cube-len))
        [loc (rotation normal) (rotation heading)]
      :else [loc' normal heading])))

(defn valid-grid-loc? [[[r c :as loc] _]]
  (and
    (< -1 r grid-height)
    (< -1 c grid-width)
    (not= \space (get-in grid loc))))

(defn both-forward [[g-loc g-head] [c-loc normal c-head]]
  [[(u/++ g-loc g-head) g-head]
   (march c-loc normal c-head)])

(defn both-left [[g-loc g-head] [c-loc normal c-head]]
  (let [g-head' (grid-left g-head)
        c-head' (spin-left c-head normal)]
    [[(u/++ g-loc g-head') g-head']
     (march c-loc normal c-head')]))

(defn both-right [[g-loc g-head] [c-loc normal c-head]]
  (let [g-head' (grid-right g-head)
        c-head' (spin-right c-head normal)]
    [[(u/++ g-loc g-head') g-head']
     (march c-loc normal c-head')]))

;; Build mapping between cube coordinates and grid coordinates

(def init-grid-loc [0 (u/index-find (first grid) \.)])
(def init-grid-heading [0 1])
(def init-grid-data [init-grid-loc init-grid-heading])
(def init-cube-loc [0 0 49])
(def init-cube-heading [1 0 0])
(def init-normal [0 0 1])
(def init-cube-data [init-cube-loc init-normal init-cube-heading])

(defn build-space-map []
  (let [init-q (conj clojure.lang.PersistentQueue/EMPTY [init-grid-data init-cube-data])
        init-map {}]
    (loop [q init-q
           m init-map
           i 0
           found #{}]
      (if (seq q)
        (let [[grid-data cube-data] (peek q)]
          (if (and (valid-grid-loc? grid-data) (not (found (first grid-data))))
            (recur
              (into (pop q)
                    [(both-forward grid-data cube-data)
                     (both-left grid-data cube-data)
                     (both-right grid-data cube-data)])
              (assoc m (take 2 cube-data) (first grid-data))
              (inc i)
              (conj found (first grid-data)))
            (recur (pop q) m (inc i) found)))
      m))))

(def space-map (build-space-map))


;; Final Solution - does not handle direction properly, but there are only 4 possibilites to guess.

(defn score [[r c] dir]
  (let [facing-score ({[0 1] 0 [1 0] 1 [0 -1] 2 [-1 0] 3} dir)]
    (+ (* 1000 (inc r))
       (* 4    (inc c))
       facing-score)))

(defn solve [[grid instructions]]
  (loop [xs instructions
         cube-data init-cube-data ]
    (if (seq xs)
      (let [x (first xs)
            [c-loc norm c-head] cube-data]
        (case x
          \R (recur (rest xs) [c-loc norm (spin-right c-head norm)])
          \L (recur (rest xs) [c-loc norm (spin-left c-head norm)])
          (let [free? (= \. (get-in grid (space-map (take 2 (march c-loc norm c-head)))))]
            (recur
              (if (= x 1) (rest xs) (conj (rest xs) (dec x)))
              (if free? (march c-loc norm c-head) [c-loc norm c-head])))))
      (score (space-map (take 2 cube-data)) [-1 0]))))

(solve [grid instructions])
