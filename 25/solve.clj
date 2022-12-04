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

(defn run-program [instructions initial-a]
  (loop [rs {:a initial-a :b 0 :c 0 :d 0} ; registers
         ic 0                             ; instruction counter
         b []]                            ; output buffer
    (if (= 12 (count b)) b
    (if-let [instruction (get instructions ic)]
      (m/match instruction
        [:cpyrr x y] (recur (assoc rs y (rs x))    (inc ic)                                       b)
        [:cpyir x y] (recur (assoc rs y x)         (inc ic)                                       b)
        [:inc x]     (recur (update-in rs [x] inc) (inc ic)                                       b)
        [:dec x]     (recur (update-in rs [x] dec) (inc ic)                                       b)
        [:jnzrr x y] (recur rs                     (if (non-zero? (rs x)) (+ ic (rs y)) (inc ic)) b)
        [:jnzri x y] (recur rs                     (if (non-zero? (rs x)) (+ ic y) (inc ic))      b)
        [:jnzir x y] (recur rs                     (if (non-zero? x) (+ ic (rs y)) (inc ic))      b)
        [:jnzii x y] (recur rs                     (if (non-zero? x) (+ ic y) (inc ic))           b)
        [:out x]     (recur rs                     (inc ic)                                       (conj b (rs x))))
      rs))))

(def input (-> (slurp "input.dat") parse-input))

(loop [i 1]
  (let [result (run-program input i)]
    (if (= "010101010101" (apply str result))
      (prn i result)
      (do
        (prn result)
        (recur (inc i))))))
