(ns advent.2016.2
  (:require [clojure.string :as s]))

(def grid [[\. \. \. \. \. \. \.]
           [\. \. \.  1 \. \. \.]
           [\. \.  2  3  4 \. \.]
           [\.  5  6  7  8  9 \.]
           [\. \. \A \B \C \. \.]
           [\. \. \. \D \. \. \.]
           [\. \. \. \. \. \. \.]])

(def start-pos [3 1])

(defn next-pos [[curr-row curr-col :as curr-pos] direction]
  (let [target-pos
          (case direction
            \D [(inc curr-row) curr-col]
            \U [(dec curr-row) curr-col]
            \R [curr-row (inc curr-col)]
            \L [curr-row (dec curr-col)])]
    (if (not= \. (get-in grid target-pos))
      target-pos
      curr-pos)))

(defn solve [input]
  (loop [curr-pos start-pos
         instructions input
         numbers-found []]
    (if (seq instructions)
      (let [instruction (first instructions)
            new-pos (reduce next-pos curr-pos instruction)]
        (recur new-pos (rest instructions) (conj numbers-found (get-in grid new-pos))))
      numbers-found)))

(defn solve-file [input]
  (-> input
      slurp
      s/split-lines
      solve
      prn))

(solve-file "input.dat")
