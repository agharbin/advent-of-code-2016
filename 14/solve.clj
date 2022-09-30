(ns advent.2016.14)

(def ^java.security.MessageDigest md5-digester (java.security.MessageDigest/getInstance "MD5"))

(defn digest-string [^java.lang.String s]
  (.digest md5-digester (.getBytes s)))

(defn bytes-as-hex [xs] (apply str (map #(format "%02x" %) xs)))

(defn first-nibble [b]
  (bit-and 2r1111 (bit-shift-right b 4)))

(defn second-nibble [b]
  (bit-and 2r1111 b))

(defn bytes->nibbles [xs]
  (flatten (map (juxt first-nibble second-nibble) xs)))

(defn find-repetition [n xs]
  (if-let [triple (filter #(apply = %) (partition n 1 xs))]
    (ffirst triple)
    nil))

(defn matching-triples-indexes [seen-triples i n]
  (->> seen-triples
       keys
       (filter #(<= (- i 1000) % (dec i)))
       (filter #(= n (seen-triples %)))))

(defn multi-hash [s]
  (iterate #(bytes-as-hex (digest-string %)) s))

(defn hash-2016-times [s]
  (digest-string (first (drop 2016 (multi-hash s)))))

(defn solve [seed]
  (loop [i 0
         seen-triples {}
         found-keys #{}]
    (if (<= 64 (count found-keys))
      (last (take 64 (sort found-keys)))
      (let [nibbles (bytes->nibbles (hash-2016-times (str seed i)))
            triple (find-repetition 3 nibbles)
            five-tuple (find-repetition 5 nibbles)]
        (recur (inc i)
               (if triple (assoc seen-triples i triple) seen-triples)
               (if five-tuple
                 (into found-keys (matching-triples-indexes seen-triples i five-tuple))
                 found-keys))))))

(prn (solve "cuanljph"))
