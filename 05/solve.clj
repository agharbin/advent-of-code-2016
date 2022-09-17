(ns advent.2016.5)

(def input "wtnhxymk")

;; Digestion

(def ^java.security.MessageDigest md5-digester (java.security.MessageDigest/getInstance "MD5"))

(defn digest-string [^java.lang.String s]
  (.digest md5-digester (.getBytes s)))

(defn bytes-as-hex [byte-seq]
  (loop [xs byte-seq
         result []]
    (if (seq xs)
      (let [next-byte (first xs)
            next-byte-as-str (format "%02X" next-byte)]
        (recur (rest xs) (conj result next-byte-as-str)))
      (apply str result))))

;; Solution 1

(defn valid-hash? [xs]
  (and
    (= [0 0] (take 2 xs))
    (<= 0 (nth xs 2) 15)))

(defn solve [input]
  (loop [i 0
         result []]
    (if (= 8 (count result))
      (apply str result)
      (let [next-input (str input i)
            next-hash (digest-string next-input)]
        (if (valid-hash? next-hash)
          (recur (inc i) (conj result (subs (bytes-as-hex next-hash) 5 6)))
          (recur (inc i) result))))))

;; Solution 2

(defn valid-hash-2? [seen-positions xs]
  (and
    (= [0 0] (take 2 xs))
    (<= 0 (nth xs 2) 7)
    (not (contains? seen-positions (nth xs 2)))))

(defn print-partial [result]
  (println (apply str (for [i (range 8)] (get result i "_")))))

(defn solve-2 [input]
  (loop [i 0
         result (sorted-map)
         seen-positions #{}]
    (if (= 8 (count result))
      (apply str (vals result))
      (let [next-input (str input i)
            next-hash (digest-string next-input)]
        (if (valid-hash-2? seen-positions next-hash)
          (let [position (nth next-hash 2)
                next-char (subs (bytes-as-hex next-hash) 6 7)]
            (print-partial (assoc result position next-char))
            (recur (inc i) (assoc result position next-char) (conj seen-positions position)))
          (recur (inc i) result seen-positions))))))

(solve-2 input)
