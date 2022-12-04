(ns advent.2016.25
  (:require [clojure.string :as s]
            [clojure.core.match :as m]))

;; Input Handling

(def register? #{"a" "b" "c" "d"})

(defn parse-line [line]
  (let [split-line (s/split line #"\s+")]
    (m/match split-line
      ["cpy" (x :guard register?) y]                    [:cpyrr (keyword x) (keyword y)]
      ["cpy" x y]                                       [:cpyir (parse-long x) (keyword y)]
      ["inc" x]                                         [:inc   (keyword x)]
      ["dec" x]                                         [:dec   (keyword x)]
      ["jnz" (x :guard register?) (y :guard register?)] [:jnzrr (keyword x) (keyword y)]
      ["jnz" (x :guard register?) y]                    [:jnzri (keyword x) (parse-long y)]
      ["jnz" x (y :guard register?)]                    [:jnzir (parse-long x) (keyword y)]
      ["jnz" x y]                                       [:jnzii (parse-long x) (parse-long y)]
      ["out" x]                                         [:out   (keyword x)])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       vec))

;; Logic

(def non-zero? (complement zero?))

(defn run-program [instructions a]
  (loop [rs {:a a :b 0 :c 0 :d 0}
         ic 0
         c 12
         b []]
    (if (zero? c) b
    (if-let [instruction (get is ic)]
      (m/match instruction
        [:cpyrr x y] (recur (assoc rs y (rs x))    (inc ic)                                       c       b)
        [:cpyir x y] (recur (assoc rs y x)         (inc ic)                                       c       b)
        [:inc x]     (recur (update-in rs [x] inc) (inc ic)                                       c       b)
        [:dec x]     (recur (update-in rs [x] dec) (inc ic)                                       c       b)
        [:jnzrr x y] (recur rs                     (if (non-zero? (rs x)) (+ ic (rs y)) (inc ic)) c       b)
        [:jnzri x y] (recur rs                     (if (non-zero? (rs x)) (+ ic y) (inc ic))      c       b)
        [:jnzir x y] (recur rs                     (if (non-zero? x) (+ ic (rs y)) (inc ic))      c       b)
        [:jnzii x y] (recur rs                     (if (non-zero? x) (+ ic y) (inc ic))           c       b)
        [:out x]     (recur rs                     (inc ic)                                       (dec c) (conj b (rs x))))
      rs))))

(def input (-> (slurp "input.dat") parse-input))

(loop [i 1]
  (let [result (run-program input i)]
    (if (= "010101010101" (apply str result))
      (prn i result)
      (do
        (prn result)
        (recur (inc i))))))
