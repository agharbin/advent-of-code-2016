(ns advent.2016.16)

(def input "01111001100111011")
(def disk-size 35651584)

(defn expand [x]
  (let [a x
        b (apply str (replace {\0 \1 \1 \0} (reverse x)))]
    (str a "0" b)))

(defn fill-disk [init-val]
  (->> init-val
       (iterate expand)
       (drop-while #(< (count %) disk-size))
       first
       (take disk-size)
       (apply str)))

(defn checksum [xs]
  (if (odd? (count xs))
    xs
    (->> (partition 2 xs)
         (map {[\0 \0] \1 [\1 \1] \1 [\0 \1] \0 [\1 \0] \0})
         (apply str)
         checksum)))

(checksum (fill-disk input))
