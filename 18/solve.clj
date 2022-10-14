(ns advent.2016.18
  (:require [clojure.string :as s]))

(defn next-tile [prev-tiles]
  (case prev-tiles
    [\^ \^ \.] \^
    [\. \^ \^] \^
    [\^ \. \.] \^
    [\. \. \^] \^
    \.))

(defn triples [input]
  (partition 3 1 (str "." input ".")))

(defn next-row [row]
  (apply str (map next-tile (triples row))))

(defn solve-file [input]
  (->> (slurp input)
       s/trim
       (iterate next-row)
       (take 400000)
       (apply str)
       frequencies))

(solve-file "input.dat")
