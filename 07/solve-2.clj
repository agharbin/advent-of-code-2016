(ns advent.2016.7
  (:require [clojure.string :as s]))

;; Input Handling

(def hypernet-seq-regex #"\[(\w+)\]")

(defn parse-line [line]
  (let [hypernet-segs (map second (re-seq hypernet-seq-regex line))
        supernet-segs (s/split line hypernet-seq-regex)]
    [(apply vector hypernet-segs) supernet-segs]))

(defn parse-input [input]
  (map parse-line (s/split-lines input)))

;; Logic

(defn is-aba? [xs]
  (and (=    (nth xs 0) (nth xs 2))
       (not= (nth xs 0) (nth xs 1))))

(defn extract-abas [xs]
  (->> xs
       (partition 3 1)
       (filter is-aba?)
       (map #(apply str %))))

(defn aba->bab [xs]
  (let [a (first xs)
        b (second xs)]
    (str b a b)))

(defn supports-ssl? [[hypernet supernet]]
  (let [abas (mapcat extract-abas supernet)
        babs (map aba->bab abas)
        bab-patterns (map re-pattern babs)
        pairs (for [x bab-patterns y hypernet] [x y])]
    (boolean (some (fn [[re s]] (re-find re s)) pairs))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       (filter supports-ssl?)
       count))

(solve-file "input.dat")
