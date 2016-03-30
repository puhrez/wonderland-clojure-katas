(ns alphabet-cipher.coder)
(def lower-case-index 97)
(def alphabet-len 26)
(def alphabet (map char
                   (range lower-case-index
                          (+ lower-case-index alphabet-len))))
(defn shift
  [xs & [offset]]
  (let [offset (or offset 1)]
    (cond
      (pos? offset) (shift (conj (vec (rest xs)) (first xs))
                             (dec offset))
      (neg? offset) (shift (conj (butlast xs) (last xs))
                           (inc offset))
      :else xs)))


(def alphabet-matrix
  (zipmap alphabet
          (for [l alphabet
            :let [offset (.indexOf alphabet l)]]
            (shift alphabet offset))))

(defn decode [keyword message]
    "loops over the length of the message
    and indexes into alphabet-matrix
    i = char at index of current letter in cycle of keyword
    j = index of the current letter on alphabet-matrix row i"
  (->> (map-indexed (fn [i x]
                      (let [row (get alphabet-matrix
                                     (nth (cycle keyword) i))
                            j (.indexOf row x)]
                        (nth alphabet j)))
                    (vec message))
       (apply str)))

(defn encode [keyword message]
    "loops over the length of the message
    and indexes into alphabet-matrix
    j = index of the current letter in the alphabet
    i = char at j in cycle of keyword"
  (->> (map-indexed (fn [i x]
                      (let [j (.indexOf alphabet x)
                            row (get alphabet-matrix
                                     (nth (cycle keyword) i))]
                        (nth row j)))
                    (vec message))
       (apply str)))

(defn clean-keyword [cipher message k]
  "works in O(n) but can probably be made better"
  (let [possibilities (rest (reductions conj [(first k)] (rest k)))]
    (loop [head (first possibilities)
           tail (rest possibilities)]
      (if (= message (decode (apply str head) cipher))
        head
        (recur (first tail) (rest tail))))))

(defn decipher [cipher message]
  (let [cleaner (partial clean-keyword cipher message)]
    (->> (map-indexed (fn [i x]
                        (let [col (.indexOf alphabet x)
                              cipher-index (.indexOf alphabet
                                                     (nth (cycle cipher) i))
                              diff (- cipher-index col)
                              row (shift alphabet diff)]
                          (first row)))
                      (vec message))
         cleaner
         (apply str))))
