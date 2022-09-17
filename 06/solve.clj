(ns advent.2016.6
  (:require [clojure.string :as s]))

;; Solution 1

(defn solve-column-1 [c]
  (let [freqs (frequencies c)
        highest-freq (apply max (vals freqs))
        max-pair (filter (fn [[k v]] (= v highest-freq)) freqs)]
    (first (first max-pair))))

;; Solution 2

(defn solve-column-2 [c]
  (let [freqs (frequencies c)
        lowest-freq (apply min (vals freqs))
        min-pair (filter (fn [[k v]] (= v lowest-freq)) freqs)]
    (first (first min-pair))))

(defn solve [input]
  (let [columns (apply map vector input)]
    (apply str (map solve-column-2 columns))))

(defn solve-file [input]
  (-> input
      slurp
      s/split-lines
      solve
      prn))

(solve-file "input.dat")
