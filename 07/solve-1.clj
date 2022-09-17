(ns advent.2016.7
  (:require [clojure.string :as s]))

(def hypernet-seq-regex #"(\[\w+\])")

(defn parse-line [line]
  (let [hypernet-seqs (map second (re-seq hypernet-seq-regex line))
        joined-hypernet-seqs (s/join "|" hypernet-seqs)
        cleaned-hypernet-segments (-> joined-hypernet-seqs (s/replace "[" "") (s/replace "]" ""))
        supernet-segments (s/replace line hypernet-seq-regex "|")]
    [cleaned-hypernet-segments supernet-segments]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(defn is-abba? [xs]
  (and (=    (nth xs 0) (nth xs 3))
       (=    (nth xs 1) (nth xs 2))
       (not= (nth xs 0) (nth xs 1))))

(defn has-abba? [xs]
  (boolean (some is-abba? (partition 4 1 xs))))

(defn supports-tls? [[hypernet supernet]]
  (and (has-abba? supernet)
       (not (has-abba? hypernet))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       (filter supports-tls?)
       count))

(solve-file "input.dat")
