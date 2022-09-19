(ns advent.2016.8
  (:require [clojure.string :as s]))

;; Input Handling

(def rect-re #"rect (\d+)x(\d+)")
(def rotate-row-re #"rotate row y=(\d+) by (\d+)")
(def rotate-col-re #"rotate column x=(\d+) by (\d+)")

(defn parse-line [line]
  (if-let [[_ x y] (re-matches rect-re line)]
    [:rect (parse-long x) (parse-long y)]
    (if-let [[_ x y] (re-matches rotate-row-re line)]
      [:row (parse-long x) (parse-long y)]
      (let [[_ x y] (re-matches rotate-col-re line)] [:col (parse-long x) (parse-long y)]))))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def width 50)
(def height 6)
(def grid (into {} (for [r (range height) c (range width)] [[r c] false])))

(defn print-grid [g]
  (loop [r 0]
    (if (< r height)
      (do
        (let [cols (for [c (range width)] (if (g [r c]) \# \.))]
          (println (apply str cols)))
        (recur (inc r)))
      nil)))

(defn rect-instr [g x y]
  (let [coords (for [r (range y) c (range x)] [r c])]
    (reduce #(assoc %1 %2 true) g coords)))

(defn rotated-col [c dist]
  (mod (- c dist) width))

(defn row-instr [initial-grid r dist]
  (loop [grid initial-grid
         cols (range width)]
    (if (seq cols)
      (let [c (first cols)
            c' (rotated-col c dist)]
        (recur (assoc grid [r c] (initial-grid [r c'])) (rest cols)))
      grid)))

(defn rotated-row [r dist]
  (mod (- r dist) height))

(defn col-instr [initial-grid c dist]
  (loop [grid initial-grid
         rows (range height)]
    (if (seq rows)
      (let [r (first rows)
            r' (rotated-row r dist)]
        (recur (assoc grid [r c] (initial-grid [r' c])) (rest rows)))
      grid)))

(defn apply-instr [g instr]
  (let [[opcode x y] instr]
    (case opcode
      :rect (rect-instr g x y)
      :row (row-instr g x y)
      :col (col-instr g x y))))

(defn solve [input]
  (reduce apply-instr grid input))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       print-grid))

(solve-file "input.dat")
