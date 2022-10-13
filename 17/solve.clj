(ns advent.2016.17)

(def seed "qtetzkpl")

(def ^java.security.MessageDigest md5-digester (java.security.MessageDigest/getInstance "MD5"))

(defn digest-string [^java.lang.String s]
  (.digest md5-digester (.getBytes s)))

(defn bytes-as-hex [byte-seq]
  (apply str (map #(format "%02X" %) byte-seq)))

(def open-chars #{\B \C \D \E \F})

(defn directions-open [s]
  (let [[up down left right] (->> s digest-string bytes-as-hex (map open-chars) (map boolean))]
    [up down left right]))

(defn legal-moves [s]
  (let [path-string (apply str seed (:path s))
        direction-unlocked (directions-open path-string)
        [r c] (s :position)
        adjacent-spaces [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]]
        zipped (map vector [\U \D \L \R] direction-unlocked adjacent-spaces)]
    (mapcat
      (fn [[dir unlocked next-position]]
         (if (and unlocked (every? #(<= 0 % 3) next-position))
           [{:position next-position :path (conj (:path s) dir)}]
           []))
      zipped)))

(defn goal? [s]
  (= (:position s) [3 3]))

(defn bfs [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}]
    (when (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal? state) (apply str (:path state))
          (seen-states state) (recur (pop q) seen-states)
          :else
            (let [candidates (legal-moves state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state))))))))

(defn bfs-2 [start-state]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         seen-states #{}
         longest-path 0]
    (if (seq q)
      (let [[dist state] (peek q)]
        (cond
          (goal? state) (recur (pop q) seen-states (max longest-path dist))
          (seen-states state) (recur (pop q) seen-states longest-path)
          :else
            (let [candidates (legal-moves state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (conj seen-states state)
                longest-path))))
      longest-path)))

(bfs-2 {:position [0 0] :path []})
