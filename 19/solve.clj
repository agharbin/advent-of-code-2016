(ns advent.2016.19)

(def input 3018458)

(defn next-index [i] (mod (inc i) input))

(defn solve []
  (let [array (java.util.BitSet.)]
    (loop [index 0
           to-skip 1
           num-set 0]
      (cond
        (= num-set (dec input)) (inc (.nextClearBit array 0))
        (.get array index) (recur (next-index index) to-skip num-set)
        (zero? to-skip) (do (.set array index) (recur (next-index index) 1 (inc num-set)))
        :else (recur (next-index index) (dec to-skip) num-set)))))

(defn half-circle [input]
  (int (Math/floor (/ input 2))))

(defn solve-2 []
  (let [array (java.util.BitSet.)]
    (loop [index (half-circle input)
           cadence (cycle (if (odd? input) [true false true] [true true false]))
           num-set 0]
      (if (= num-set (dec input))
        (inc (.nextClearBit array 0))
        (cond
          (true? (.get array index)) (recur (next-index index) cadence num-set)
          (true? (first cadence))
            (do
              (.set array index)
              (recur (next-index index) (next cadence) (inc num-set)))
          :else (recur (next-index index) (next cadence) num-set))))))

(solve-2)
