(ns advent.2016
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
      ["tgl" x]                                         [:tgl   (keyword x)])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       vec))

;; Logic

(def initial-registers   {:a 12 :b 0 :c 0 :d 0})
(def non-zero? (complement zero?))

(defn toggled [instruction]
  (m/match instruction
        [:cpyrr x y]  [:jnzrr x y]
        [:cpyir x y]  [:jnzir x y]
        [:inc x]      [:dec x]
        [:dec x]      [:inc x]
        [:jnzrr x y]  [:cpyrr x y]
        [:jnzri x y]  [:skip x y]
        [:jnzir x y]  [:cpyir x y]
        [:jnzii x y]  [:skip x y]
        [:tgl x]      [:inc x]))

(defn toggle-at [instructions index]
  (if (contains? instructions index)
    (assoc instructions index (toggled (instructions index)))
    instructions))

(defn run-program [instructions]
  (loop [rs initial-registers
         ic 0
         is instructions]
    (prn ic (is ic) rs)
    (if-let [instruction (get is ic)]
      (m/match instruction
        [:cpyrr x y] (recur (assoc rs y (rs x))    (inc ic)                                       is)
        [:cpyir x y] (recur (assoc rs y x)         (inc ic)                                       is)
        [:inc x]     (recur (update-in rs [x] inc) (inc ic)                                       is)
        [:dec x]     (recur (update-in rs [x] dec) (inc ic)                                       is)
        [:jnzrr x y] (recur rs                     (if (non-zero? (rs x)) (+ ic (rs y)) (inc ic)) is)
        [:jnzri x y] (recur rs                     (if (non-zero? (rs x)) (+ ic y) (inc ic))      is)
        [:jnzir x y] (recur rs                     (if (non-zero? x) (+ ic (rs y)) (inc ic))      is)
        [:jnzii x y] (recur rs                     (if (non-zero? x) (+ ic y) (inc ic))           is)
        [:skip _ _]  (recur rs                     (inc ic)                                       is)
        [:tgl x]     (recur rs                     (inc ic)              (toggle-at is (+ ic (rs x)))))
      rs)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      run-program
      prn))

(solve-file "input.dat")
