(ns advent.2016.5)

(def input "wtnhxymk")

;; Digestion

(def md5-digester (java.security.MessageDigest/getInstance "MD5"))

(defn digest-string [s]
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
    (when (= 0 (mod i 100000)) (printf "Iteration: %d, found %d\n" i (count result)))
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

(defn solve-2 [input]
  (loop [i 0
         result (sorted-map)
         seen-positions #{}]
    (when (= 0 (mod i 100000)) (printf "Iteration: %d, found %d\n" i (count result)))
    (if (= 8 (count result))
      (apply str (vals result))
      (let [next-input (str input i)
            next-hash (digest-string next-input)]
        (if (valid-hash-2? seen-positions next-hash)
          (let [position (nth next-hash 2)
                next-char (subs (bytes-as-hex next-hash) 6 7)]
            (recur (inc i) (assoc result position next-char) (conj seen-positions position)))
          (recur (inc i) result seen-positions))))))

(solve input)
