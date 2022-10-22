(ns advent.2016.20
  (:require [clojure.string :as s]))

;; Input Handling

(defn parse-line [input]
  (let [[x y] (s/split input #"-")]
    [(parse-long x) (parse-long y)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn collapse [xs]
  (if (seq xs)
    (let [[[low-1 high-1] [low-2 high-2]] (take 2 xs)]
      (cond
        (nil? low-2) (list [low-1 high-1])
        (< 1 (- low-2 high-1)) (concat [[low-1 high-1]] (collapse (drop 1 xs)))
        :else (collapse (conj (drop 2 xs) [(min low-1 high-1 low-2) (max high-1 low-2 high-2)]))))
    '()))

(defn solve [input]
  (->> input
       (sort-by first)
       collapse
       (partition 2 1)
       (map (fn [[[_ x] [y _]]] (- y x 1)))
       (apply +)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file "input.dat")
