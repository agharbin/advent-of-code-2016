(ns advent.2016.13)

(def input 1352)
(def goal-square [31 39])

(defn goal? [s]
  (= s goal-square))

(defn open-space? [[x y]]
  (let [n (+ input (* x x) (* 3 x) (* 2 x y) y (* y y))]
    (->> (range 32) ; Assume in fits in a 32-bit int
         (map #(bit-test n %))
         (filter true?)
         count
         even?)))

(def non-negative? (complement neg?))

(defn possible-next-states [[x y]]
  (let [directional-moves [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
        on-board-moves (filter #(every? non-negative? %) directional-moves)]
    (filter open-space? on-board-moves)))

(defn bfs [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (when (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal? state) dist
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (possible-next-states state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state))))))))

(defn bfs-2 [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (if (seq q)
      (let [[dist state] (peek q)]
        (cond
          (< 50 dist) (recur (pop q) seen-states)
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (possible-next-states state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state)))))
      (count seen-states))))

(bfs-2 [1 1])
