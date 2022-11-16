(ns advent.2016.22.1
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

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
       (map parse-line)))

;; Logic

(defn solve [input]
  (loop [by-used-ascending  (->> (sort-by :used input) (drop-while #(zero? (:used %))))
         by-avail-ascending (sort-by :avail input)
         result 0]
    (if (seq by-used-ascending)
      (let [this-node (first by-used-ascending)
            targets-big-enough (drop-while #(<= (:avail %) (:used this-node)) by-avail-ascending)
            num-big-enough (count targets-big-enough)
            fits-in-self? (<= (:used this-node) (:avail this-node))]
        (recur
          (rest by-used-ascending)
          targets-big-enough
          (if fits-in-self?
            (+ result num-big-enough -1) ; We counted ourselves improperly
            (+ result num-big-enough)))) ; We did not count ourselves
      result)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      pp/pprint))

(solve-file "input.dat")
