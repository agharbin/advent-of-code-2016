(ns advent.2016.4
  (:require [clojure.string :as s]))

;; Handle Input

(def regex #"([a-z-]+)(\d+)\[(.*)\]")

(defn parse-line [input]
  (let [[_ text sector chksum]  (re-matches regex input)]
    [text (parse-long sector) chksum]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn sort-func [[k1 v1] [k2 v2]]
  (if (= v1 v2)
    (- (int k1) (int k2))
    (- v2 v1)))

(defn checksum-valid? [[input _ chksum]]
  (let [dashes-removed (apply str (filter #(not= \- %) input))
        freq (frequencies dashes-removed)
        sorted (sort-by identity sort-func freq)
        expected-chars (take 5 (map first sorted))
        expected-str (apply str expected-chars)]
    (= expected-str chksum)))

(defn rotate [c x]
  (let [nth-char (- (int c) (int \a))
        new-char-num (mod (+ nth-char x) 26)]
    (char (+ (int \a) new-char-num))))

(defn decrypt [[input sector _]]
  (apply str (for [c input] (if (= c \-) \space (rotate c sector)))))

(defn solve [input]
  (->> input
       (filter checksum-valid?)
       (filter #(re-matches #".*northpole.*" (decrypt %)))
       (map (juxt second decrypt))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve))

(solve-file "input.dat")
