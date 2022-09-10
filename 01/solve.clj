(ns advent.2016.1
  (:require [clojure.pprint :as pp]))

;; Input Handling

(def regex #"(\w)(\d+)")

(defn parse-input [input]
  (for [[_ turn dist] (re-seq regex input)]
    [(keyword turn) (parse-long dist)]))

;; Logic

(defn rotate [turn [x y]]
  (case turn
    :R [y (* -1 x)]
    :L [(* -1 y) x]))

(defn grid-dist [position]
  (apply + (map abs position)))

;; Solution 1

(defn next-position [[curr-pos facing-dir] [turn dist]]
  (let [new-facing (rotate turn facing-dir)
        position-delta (mapv #(* % dist) new-facing)
        new-position (map + curr-pos position-delta)]
    [new-position new-facing]))

(defn solve [input]
  (let [[final-pos _] (reduce next-position [[0 0] [0 1]] input)]
    (grid-dist final-pos)))

;; Solution 2

(defn advance [curr-pos facing-dir]
  (map + curr-pos facing-dir))

(def is-turn? #{:R :L})

(defn solve2 [input]
  (loop [curr-pos [0 0]
         curr-facing [0 1]
         visited #{}
         moves (seq (flatten input))]
    (if (visited curr-pos)
      (grid-dist curr-pos)
      (let [move (first moves)]
        (if (is-turn? move)
          (recur curr-pos (rotate move curr-facing) visited (rest moves))
          (recur (advance curr-pos curr-facing)
                 curr-facing
                 (conj visited curr-pos)
                 (if (= 1 move)
                   (rest moves)
                   (conj (rest moves) (dec move)))))))))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve2
      pp/pprint))

(solve-file "input.dat")
