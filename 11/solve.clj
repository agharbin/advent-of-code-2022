(ns advent.2022.11)

(def sample
  {0
   {:items [79 98]
    :op (fn [x] (* 19 x))
    :test (fn [x] (= 0 (mod x 23)))
    :mod (fn [x] (mod x 23))
    :seen 0
    :true 2
    :false 3}
   1
   {:items [54 65 75 74]
    :op (fn [x] (+ 6 x))
    :test (fn [x] (= 0 (mod x 19)))
    :mod (fn [x] (mod x 19))
    :seen 0
    :true 2
    :false 0}
   2
   {:items [79 60 97]
    :op (fn [x] (* x x))
    :test (fn [x] (= 0 (mod x 13)))
    :mod (fn [x] (mod x 13))
    :seen 0
    :true 1
    :false 3}
   3
   {:items [74]
    :op (fn [x] (+ 3 x))
    :test (fn [x] (= 0 (mod x 17)))
    :mod (fn [x] (mod x 17))
    :seen 0
    :true 0
    :false 1}})

(def input
  {0
   {:items [75 63]
    :op (fn [x] (* 3 x))
    :test (fn [x] (= 0 (mod x 11)))
    :seen 0
    :true 7
    :false 2}
   1
   {:items [65 79 98 77 56 54 83 94]
    :op (fn [x] (+ 3 x))
    :test (fn [x] (= 0 (mod x 2)))
    :seen 0
    :true 2
    :false 0}
   2
   {:items [66]
    :op (fn [x] (+ 5 x))
    :test (fn [x] (= 0 (mod x 5)))
    :seen 0
    :true 7
    :false 5}
   3
   {:items [51 89 90]
    :op (fn [x] (* 19 x))
    :test (fn [x] (= 0 (mod x 7)))
    :seen 0
    :true 6
    :false 4}
   4
   {:items [75 94 66 90 77 82 61]
    :op (fn [x] (+ 1 x))
    :test (fn [x] (= 0 (mod x 17)))
    :seen 0
    :true 6
    :false 1}
   5
   {:items [53 76 59 92 95]
    :op (fn [x] (+ 2 x))
    :test (fn [x] (= 0 (mod x 19)))
    :seen 0
    :true 4
    :false 3}
   6
   {:items [81 61 75 89 70 92]
    :op (fn [x] (* x x))
    :test (fn [x] (= 0 (mod x 3)))
    :seen 0
    :true 0
    :false 1}
   7
   {:items [81 86 62 87]
    :op (fn [x] (+ 8 x))
    :test (fn [x] (= 0 (mod x 13)))
    :seen 0
    :true 3
    :false 5}})

(def mod-const 9699690)

(defn passing-items [m]
  (->> (:items m)
       (map (:op m))
       (map #(mod % mod-const))
       (filter (:test m))))

(defn non-passing-items [m]
  (->> (:items m)
       (map (:op m))
       (map #(mod % mod-const))
       (filter (complement (:test m)))))

(defn solve [input]
  (reduce (fn [state idx]
            (let [monkey (state idx)
                  passing (passing-items monkey)
                  non-passing (non-passing-items monkey)]
              (-> state
                  (update-in [(monkey :true) :items] #(vec (concat % passing)))
                  (update-in [idx :seen] #(+ % (count passing)))
                  (update-in [(monkey :false) :items] #(vec (concat % non-passing)))
                  (update-in [idx :seen] #(+ % (count non-passing)))
                  (assoc-in [idx :items] []))))
          input
          (for [i (range 10000) m (-> input keys count range)] m)))

(->> (solve input)
     vals
     (map :seen)
     sort
     (drop 6)
     (apply *))
