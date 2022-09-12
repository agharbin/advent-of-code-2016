(ns advent.2016.3
  (:require [clojure.string :as s]))

;; Input Handling

(defn parse-line [input]
  (->> (s/split input #"\s+")
       rest
       (map parse-long)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn is-triangle? [[side1 side2 side3]]
  (and
    (< side3 (+ side1 side2))
    (< side2 (+ side1 side3))
    (< side1 (+ side2 side3))))

(defn transpose-3 [xs]
  (apply mapv vector xs))

(defn transpose-input [xs]
  (->> xs
       (partition 3)
       (map transpose-3)
       (apply concat)))

(defn solve [xs]
  (->> xs (filter is-triangle?) count))

(defn solve-file [input]
  (-> (slurp input)
      parse-input
      transpose-input
      solve
      prn))

(solve-file "input.dat")
