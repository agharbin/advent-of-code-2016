(ns advent.2016.15
  (:require [clojure.set :as cs]))

(defn rotate [[pos size] n]
  [(mod (+ n pos) size) size])

(def discs [[10 13] [15 17] [17 19] [1 7] [0 5] [1 3] [0 11]])

(def normalized (map (fn [d i] (rotate d i)) discs (range 1 (inc (count discs)))))

(defn solutions
 ([pair]
  (solutions pair 0))
 ([[x y] n]
  (lazy-seq (cons (+ (- y x) (* y n)) (solutions [x y] (inc n))))))

(apply cs/intersection (map #(into #{} (take 1000000 (solutions %))) normalized))
