(ns advent.2016.21
  (:require [clojure.string :as s]
            [clojure.core.match :as m]))

;; Input Handling

(defn parse-line [line]
  (m/match (s/split line #"\s+")
    ["swap" "position" a "with" "position" b] [:swap-position (parse-long a) (parse-long b)]
    ["swap" "letter" a "with" "letter" b]     [:swap-letter (first a) (first b)]
    ["move" "position" a "to" "position" b]   [:move-position (parse-long a) (parse-long b)]
    ["rotate" "right" a _]                    [:rotate-right (parse-long a)]
    ["rotate" "left" a _]                     [:rotate-left (parse-long a)]
    ["reverse" "positions" a "through" b]     [:reverse-positions (parse-long a) (parse-long b)]
    ["rotate" "based" "on" "position" _ _ a]  [:rotate-letter-right (first a)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn swap-position [input a b]
  (let [xs (vec input)]
    (-> xs (assoc a (xs b)) (assoc b (xs a)))))

(defn swap-letter [input a b]
  (vec (replace {a b b a} input)))

(defn move-position [input a b]
  (cond
    (= a b) (vec input)
    (< a b) (move-position (swap-position input a (inc a)) (inc a) b)
    (> a b) (move-position (swap-position input (dec a) a) (dec a) b)))

(defn rotate-right [input a]
  (->> (range (count input))
       (map #(mod (- % a) (count input)))
       (map (vec input))))

(defn rotate-left [input a]
  (->> (range (count input))
       (map #(mod (+ % a) (count input)))
       (map (vec input))))

(defn reverse-positions [input a b]
  (let [xs (vec input)
        segment-1 (subvec xs 0 a)
        segment-2 (reverse (subvec xs a (inc b)))
        segment-3 (subvec xs (inc b))]
    (concat segment-1 segment-2 segment-3)))

(defn rotate-letter-right [input a]
  (let [xs (vec input)
        index (first (filter #(= (xs %) a) (range (count xs))))
        rotate-val (if (>= index 4) (+ index 2) (inc index))]
    (rotate-right input rotate-val)))

(defn rotate-letter-left [input a]
  (->> (range (count input))
       (map #(rotate-left input %))
       (filter #(= input (rotate-letter-right % a)))
       first))

(defn next-password [password instruction]
  (m/match instruction
    [:swap-position a b]     (swap-position password a b)
    [:swap-letter a b]       (swap-letter password a b)
    [:move-position a b]     (move-position password a b)
    [:rotate-right a]        (rotate-right password a)
    [:rotate-left a]         (rotate-left password a)
    [:reverse-positions a b] (reverse-positions password a b)
    [:rotate-letter-right a] (rotate-letter-right password a)
    [:rotate-letter-left  a] (rotate-letter-left password a)))

(defn opposite-instruction [instruction]
  (m/match instruction
    [:swap-position a b]     [:swap-position a b]
    [:swap-letter a b]       [:swap-letter a b]
    [:move-position a b]     [:move-position b a]
    [:rotate-right a]        [:rotate-left a]
    [:rotate-left a]         [:rotate-right a]
    [:reverse-positions a b] [:reverse-positions a b]
    [:rotate-letter-right a] [:rotate-letter-left a]
    [:rotate-letter-left  a] [:rotate-letter-right a]))

(defn solve [s input]
  (reduce next-password s input))

(defn solve-file [input s]
  (->> (slurp input)
       parse-input
       reverse
       (map opposite-instruction)
       (solve s)
       (apply str)))

(solve-file "input.dat" "fbgdceah")
