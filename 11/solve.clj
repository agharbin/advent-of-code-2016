(ns advent.2016.11
  (:require [clojure.repl :as repl]
            [clojure.pprint :as pp]
            [clojure.math.combinatorics :as combo]))

(set! *warn-on-reflection* true)

;(def materials #{:polonium :thulium :prometheum :ruthenium :cobalt :elerium :dilithium})
(def materials #{:polonium :thulium :prometheum :ruthenium :cobalt})
(def types #{:chip :generator})
(def all-parts (for [m materials t types] [t m]))
(def max-int 9999)

(def initial-state {
  :elevator 0
  :floors [
    #{[:generator :polonium] [:generator :thulium] [:chip :thulium] [:generator :prometheum] [:generator :ruthenium] [:chip :ruthenium] [:generator :cobalt] [:chip :cobalt]}
    #{[:chip :polonium] [:chip :prometheum]}
    #{}
    #{}]})

(def initial-state-2 {
  :elevator 0
  :floors [
    #{[:generator :polonium] [:generator :thulium] [:chip :thulium] [:generator :prometheum] [:generator :ruthenium] [:chip :ruthenium] [:generator :cobalt] [:chip :cobalt] [:chip :elerium] [:generator :elerium] [:chip :dilithium] [:generator :dilithium]}
    #{[:chip :polonium] [:chip :prometheum]}
    #{}
    #{}]})

(def test-case {
  :elevator 0
  :floors [
    #{[:chip :polonium] [:chip :prometheum]}
    #{[:generator :polonium]}
    #{[:generator :prometheum]}
    #{}]})

(defn goal? [state]
  (and
    (empty? (get-in state [:floors 0]))
    (empty? (get-in state [:floors 1]))
    (empty? (get-in state [:floors 2]))))

(defn valid-floor? [xs]
  (if (boolean (some #{:generator} (map first xs)))
    (let [chips (filter #(= :chip (first %)) xs)
          matching-generators (for [c chips] [:generator (second c)])]
      (every? xs matching-generators))
    true))

(defn valid-state? [s]
  (every? valid-floor? (s :floors)))

(defn possible-objects-to-take [floor-contents]
  (into #{}
    (concat
      (map #(hash-set %) floor-contents)
      (for [i floor-contents j floor-contents :when (not= i j)] #{i j}))))

(defn possible-moves [state]
  (let [floor-num (state :elevator)
        poss-next-floors (filter #(<= 0 % 3) [(dec floor-num) (inc floor-num)])
        floor-contents (get-in state [:floors floor-num])
        poss-objects-to-move (possible-objects-to-take floor-contents)]
    (for [f poss-next-floors o poss-objects-to-move] [f o])))

(defn next-state [state [next-floor objects-to-move]]
  (let [current-floor (state :elevator)
        current-floor-contents (get-in state [:floors current-floor])
        next-floor-contents (get-in state [:floors next-floor])]
    (-> state
        (assoc-in [:elevator] next-floor)
        (assoc-in [:floors current-floor] (reduce disj current-floor-contents objects-to-move))
        (assoc-in [:floors next-floor] (reduce conj next-floor-contents objects-to-move)))))

(defn floor-permutation [old->new floor]
  (into #{} (for [[t m] floor] [t (old->new m)])))

(defn state-permutation [state p]
  (let [old->new (zipmap materials p)]
    {:elevator (state :elevator)
     :floors (map (partial floor-permutation old->new) (state :floors))}))

(defn state-permutations [state]
  (let [material-permutations (combo/permutations materials)
        permuted-states (map #(state-permutation state %) material-permutations)]
     (into #{} permuted-states)))

(defn solve [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         shortest-path-seen max-int
         seen-states #{}
         i 0]
    (if (seq q)
      (let [[dist state] (peek q)]
        (comment
        (when (zero? (mod i 100))
          (do
            (printf "iteration(%d) seen(%d)\n" i (count seen-states))
            (pp/pprint state)))
        )
        (if (some seen-states (state-permutations state))
          (recur (pop q) shortest-path-seen seen-states (inc i))
          (let [possible-moves (possible-moves state)
                possible-next-states (map #(next-state state %) possible-moves)
                valid-next-states (filter #(and (valid-state? %)
                                                (not-any? seen-states (state-permutations %)))
                                          possible-next-states)]
            (recur
              (reduce conj (pop q) (for [s valid-next-states] [(inc dist) s]))
              (if (goal? state) (min shortest-path-seen dist) shortest-path-seen)
              (conj seen-states state)
              (inc i)))))
      shortest-path-seen)))

(prn (solve initial-state))
