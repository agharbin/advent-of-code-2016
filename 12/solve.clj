(ns advent.2016.12
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.core.match :as m]))

;; Input Handling

(def register? #{"a" "b" "c" "d"})

(defn parse-line [line]
  (let [split-line (s/split line #"\s+")]
    (m/match split-line
      ["cpy" (x :guard register?) y] [:cpyr (keyword x) (keyword y)]
      ["cpy" x y]                    [:cpyi (parse-long x) (keyword y)]
      ["inc" x]                      [:inc  (keyword x)]
      ["dec" x]                      [:dec  (keyword x)]
      ["jnz" (x :guard register?) y] [:jnzr (keyword x) (parse-long y)]
      ["jnz" x y]                    [:jnzi (parse-long x) (parse-long y)])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       vec))

;; Logic

(def initial-registers   {:a 0 :b 0 :c 0 :d 0})
(def initial-registers-2 {:a 0 :b 0 :c 1 :d 0})
(def non-zero? (complement zero?))

(defn run-program [instructions]
  (loop [rs initial-registers
         ic 0]
    (if-let [instruction (get instructions ic)]
      (m/match instruction
        [:cpyr x y] (recur (assoc rs y (rs x))    (inc ic))
        [:cpyi x y] (recur (assoc rs y x)         (inc ic))
        [:inc x]    (recur (update-in rs [x] inc) (inc ic))
        [:dec x]    (recur (update-in rs [x] dec) (inc ic))
        [:jnzr x y] (recur rs                     (if (non-zero? (rs x)) (+ ic y) (inc ic)))
        [:jnzi x y] (recur rs                     (if (non-zero? x) (+ ic y) (inc ic))))
      rs)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      run-program
      pp/pprint))

(solve-file "input.dat")
