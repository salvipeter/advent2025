(def data
  (with [f (file/open "adv03.txt")]
        (map |(peg/match ~(some (number :d)) $)
             (file/lines f))))

(defn max-index
  "Index of first maximal value"
  [arr]
  (index-of (max-of arr) arr))

(defn activation [k bank]
  (var jolt 0)
  (var i 0)
  (let [n (length bank)]
    (loop [j :down-to [(dec k) 0]]
      (+= i (max-index (array/slice bank i (- n j))))
      (set jolt (+ (* jolt 10) (bank i)))
      (++ i)))
  jolt)

(defn joltage [batteries banks]
  (sum (map |(activation batteries $) banks)))

(print (joltage 2 data))
(print (joltage 12 data))
