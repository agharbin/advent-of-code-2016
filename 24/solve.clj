(ns advent.2016.24
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

;; Input Handling

(defn parse-input [input]
  "Text representation of grid to vector representation"
  (->> input
       s/split-lines
       (map seq)
       (mapv vec)))

;; Find relevant locations on map

(defn find-nums [input]
  "Return all chars in the grid aside from walls and floors with their positions"
  (filter some?
    (for [row (range (count input))
          col (range (count (first input)))]
        (if (#{\# \.} (get-in input [row col]))
          nil
          {:position [row col] :symbol (get-in input [row col])}))))

(def locations (->> (slurp "input.dat") parse-input find-nums (into #{})))

;; Find distances between relevant locations using BFS

(defn is-location [goal-symbol]
  "Returns a predicate that indicates when we are at a specific location"
  (fn [{:keys [grid position]}]
    (= goal-symbol (get-in grid position))))

(defn possible-next-states [{:keys [grid position]}]
  "Next legal state generator for BFS"
  (let [[r c] position]
    (for [[r' c'] #{[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]}
          :when (not= \# (get-in grid [r' c']))]
      {:grid grid :position [r' c']})))

(defn bfs [start-state goal-test]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (when (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal-test state) dist
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (possible-next-states state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state))))))))

(defn find-distances [input]
  "Generate pair-wise distances between given locations"
  (for [i locations
        j locations
        :when (not= (:symbol i) (:symbol j))]
    (let [start (:position i)
          start-symbol (:symbol i)
          goal-symbol (:symbol j)
          goal-condition (is-location goal-symbol)]
      [[start-symbol goal-symbol] (bfs {:grid input :position start} goal-condition)])))

(def distances (->> (slurp "input.dat") parse-input find-distances (into {})))

;; Find length of tours

(defn path-length [xs]
  (->> (partition 2 1 xs)
       (map distances)
       (apply +)))

(def all-symbols (map :symbol locations))
(def all-but-zero (rest (sort all-symbols)))

;; Part 1

(apply min (for [p (combo/permutations all-symbols)] (path-length p)))

;; Part 2
(apply min (for [p (combo/permutations all-but-zero)] (path-length (concat [\0] p [\0]))))
