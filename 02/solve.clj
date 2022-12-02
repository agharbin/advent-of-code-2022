(ns advent.2022.02.1
  (:require [clojure.string :as s]))

(defn parse-line [line-text]
  (->> (s/split line-text #" ")
       (map first)))

(defn parse-input [input]
  (->> (s/split-lines input)
       (map parse-line)))

(def choice-score {:rock 1 :paper 2 :scissors 3})
(def choice {\A :rock \X :rock \B :paper \Y :paper \C :scissors \Z :scissors})
(def result-score {:win 6 :tie 3 :lose 0})
(def result
  {[:rock :rock] :tie
   [:rock :paper] :win
   [:rock :scissors] :lose
   [:paper :rock] :lose
   [:paper :paper] :tie
   [:paper :scissors] :win
   [:scissors :rock] :win
   [:scissors :paper] :lose
   [:scissors :scissors] :tie})

(defn round-score [[them us]]
  (+ (choice-score (choice us))
     (result-score (result [(choice them) (choice us)]))))

(defn solve [input]
  (apply + (map round-score input)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       print))

(solve-file "input.dat")
