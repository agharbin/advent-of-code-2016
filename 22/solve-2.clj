(ns advent.2016.22.1
  (:require [clojure.string :as s]))

;; Data Types

(defrecord node [coords used avail])

;; Input Handling

(defn parse-node-name [n]
  (let [[_ x y] (re-matches #"/dev/grid/node-x(\d+)-y(\d+)" n)]
    [(parse-long x) (parse-long y)]))

(def re #"^([a-z0-9/-]+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%$")

(defn parse-line [line]
  (let [[_ node size used avail use%] (re-matches re line)]
    (->node (parse-node-name node) (parse-long used) (parse-long avail))))

(defn parse-input [input]
  (->> input
       s/split-lines
       (drop 2)
       (map parse-line)
       (map #(vector (:coords %) %))
       (into {})))

;; Logic

(def x-extent 36)
(def y-extent 24)

(defn render-space [{used :used}]
  (cond
    (zero? used) \0
    (< 100 used) \X
    :else \.))

(defn render [grid]
  (s/join \newline
          (for [y (range y-extent)]
            (apply str (for [x (range x-extent)] (render-space (grid [x y])))))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       render
       (spit "output.dat")))

(solve-file "input.dat")
