(ns advent.2016.11)

;; We represent the state of the world as a list of pairs [c g] where c is the
;; floor where a chip currently is, and g is the corresponding generator's location.
;; All pairs are interchangeable, so it is not necessary to track the actual material.
;; The list is kept sorted so that functionally identical configurations test as equal.

;; Cases

(def initial-state {
  :elevator 0
  :parts [[0 0] [0 0] [0 0] [1 0] [1 0]]})

(def initial-state-2 {
  :elevator 0
  :parts [[0 0] [0 0] [0 0] [0 0] [0 0] [1 0] [1 0]]})

(def test-case {
  :elevator 0
  :parts [[0 1] [0 2]]})

;; Logic

(defn goal? [{parts :parts}]
  (every? #{[3 3]} parts))

(defn legal-state? [{parts :parts}]
  (loop [xs parts]
    (if (seq xs)
      (let [[chip gen] (first xs)]
        (if (and (not= chip gen) (some #(= chip (second %)) parts))
          false
          (recur (rest xs))))
      true)))

(defn to-sorted-vectors [x]
  "Takes a sequence of integers representing part -> floor mappings and
  restores them to the original sorted pairs representation"
  (vec (sort (map vec (partition 2 x)))))

(defn possible-next-states [{parts :parts elevator :elevator}]
  (let [flattened (apply vector (flatten parts))
        indexes-at-curr-floor (filter #(= elevator (flattened %)) (range (count flattened)))
        possible-next-floors (filter #(<= 0 % 3) [(dec elevator) (inc elevator)])
        move-one (for [next-floor possible-next-floors
                       i indexes-at-curr-floor]
                   {:elevator next-floor
                    :parts (-> flattened
                               (assoc i next-floor)
                               to-sorted-vectors)})
        move-two (for [next-floor possible-next-floors
                       i indexes-at-curr-floor
                       j indexes-at-curr-floor :when (not= i j)]
                   {:elevator next-floor
                    :parts (-> flattened
                               (assoc i next-floor)
                               (assoc j next-floor)
                               to-sorted-vectors)})]
    (->> (concat move-one move-two)
         (into #{}))))

(defn bfs [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (when (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal? state) dist
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [states-to-consider (possible-next-states state)
                  legal-next-states (filter legal-state? states-to-consider)]
              (recur
                (into (pop q) (for [s legal-next-states] [(inc dist) s]))
                (conj seen-states state))))))))

(prn (bfs initial-state))
