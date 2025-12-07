(def data
  (with [f (file/open "adv07.txt")]
        (map |(string/slice $ 0 -2) (file/lines f))))

(defn count-beams [tbl part2?]
  (def height (length tbl))
  (def width (length (tbl 0)))
  (def cache @{})
  (defn aux [i j]
    (cond
      (in cache [i j]) (if part2? (cache [i j]) 0)
      (or (< i 0) (>= i width) (>= j height)) 0
      (let [result (if (= ((tbl j) i) (chr "^"))
                     (+ 1 (aux (dec i) j) (aux (inc i) j))
                     (aux i (inc j)))]
        (put cache [i j] result)
        result)))
  (aux (string/find "S" (tbl 0)) 1))

(print (count-beams data false))
(print (inc (count-beams data true)))
