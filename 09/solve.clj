(ns advent.2016.9.2
  (:require [clojure.string :as s]))

(def marker-re #"\((\d+)x(\d+)\)")

(defn first-match [p s]
  (let [matcher (re-matcher p s)
        _ (.find matcher)
        match-result (.toMatchResult matcher)]
    [(parse-long (.group match-result 1))
     (parse-long (.group match-result 2))
     (.start match-result)
     (.end match-result)]))

(defn compute-length [s]
  (if (re-find marker-re s)
    (let [[repeat-chars repeat-times start-of-marker end-of-marker] (first-match marker-re s)
          end-of-repeat-section (+ end-of-marker repeat-chars)]
      (+ start-of-marker
         (* repeat-times (- end-of-repeat-section end-of-marker))
         (compute-length (subs s end-of-repeat-section))))
    (count s)))

(defn compute-length-2 [s]
  (if (re-find marker-re s)
    (let [[repeat-chars repeat-times start-of-marker end-of-marker] (first-match marker-re s)
          end-of-repeat-section (+ end-of-marker repeat-chars)]
      (+ start-of-marker
         (* repeat-times (compute-length-2 (subs s end-of-marker end-of-repeat-section)))
         (compute-length-2 (subs s end-of-repeat-section))))
    (count s)))

(defn solve-file [input]
  (->> input
       slurp
       s/trim
       compute-length-2))

(solve-file "input.dat")
