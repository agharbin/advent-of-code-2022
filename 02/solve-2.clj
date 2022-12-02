(ns advent.2022.02.2
  (:require [clojure.string :as s]))

(defn parse-line [line-text]
  (->> (s/split line-text #" ")
       (map first)))

(defn parse-input [input-text]
  (->> (s/split-lines input-text)
       (map parse-line)))

(def choice {\A :rock \B :paper \C :scissors})
(def choice-score {:rock 1 :paper 2 :scissors 3})
(def result {\X :lose \Y :tie \Z :win})
(def result-score {:win 6 :tie 3 :lose 0})

(def our-choice
  {[:rock :tie] :rock
   [:rock :win] :paper
   [:rock :lose] :scissors
   [:paper :lose] :rock
   [:paper :tie] :paper
   [:paper :win] :scissors
   [:scissors :win] :rock
   [:scissors :lose] :paper
   [:scissors :tie] :scissors})

(defn round-score [[them result-symbol]]
  (+ (choice-score (our-choice [(choice them) (result result-symbol)]))
     (result-score (result result-symbol))))

(defn solve [input]
  (apply + (map round-score input)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       print))

(solve-file "input.dat")
