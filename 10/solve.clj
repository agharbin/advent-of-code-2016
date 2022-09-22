(ns advent.2016.10
  (:require [clojure.string :as s]))

;; Input Handling

(def bot-re #"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)")
(def value-re #"value (\d+) goes to bot (\d+)")

(defn parse-line [line]
  (if-let [[_ bot low low-num high high-num] (re-matches bot-re line)]
    [(parse-long bot) (keyword low) (parse-long low-num) (keyword high) (parse-long high-num)]
    (let [[_ value bot] (re-matches value-re line)]
      [(parse-long value) (parse-long bot)])))

(defn parse-input [input]
  (let [length-grouped-rules (->> input
                                  s/split-lines
                                  (map parse-line)
                                  (group-by count))]
    {:bot-rules (length-grouped-rules 5)
     :value-rules (length-grouped-rules 2)}))

;; Logic

(def goal [17 61])

(def empty-state {:bot {} :output {}})

(defn give-with [rule-map]
  (fn give [state target-type target-num value]
    (let [cur-val (get-in state [target-type target-num] [])
          next-val (conj cur-val value)]
      (case (count next-val)
        1 (assoc-in state [target-type target-num] next-val)
        2 (let [[low-val high-val :as pair] (sort next-val)
                [low-target low-num high-target high-num] (rule-map target-num)]
            (when (= pair goal) (println "GOAL:" target-type target-num next-val))
            (-> state
                (give low-target low-num low-val)
                (give high-target high-num high-val)
                (assoc-in [target-type target-num] [])))))))

(defn feed-initial-state [rule-map value-inputs]
  (reduce (fn [s [v b]] ((give-with rule-map) s :bot b v)) empty-state value-inputs))

(defn build-rule-map [bot-rules] (into (sorted-map) (for [b bot-rules] [(first b) (rest b)])))

(defn solve [{bot-rules :bot-rules value-rules :value-rules}]
  (let [rule-map (build-rule-map bot-rules)]
    (feed-initial-state rule-map value-rules)))

(defn compute-score [{output :output}]
  (let [nums (-> output
                 (select-keys [0 1 2])
                 vals
                 flatten)]
    (apply * nums)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      compute-score))

(solve-file "input.dat")
