(def data
  (with [f (file/open "adv04.txt")]
        (map (comp buffer string/trimr) (file/lines f))))

(defn pos [tbl i j]
  (get (get tbl i []) j (chr ".")))

(defn paper-rolls
  "Counts the paper rolls (anything except '.') around position [i j], including itself."
  [tbl i j]
  (length (seq [k :range-to [-1 1]
                l :range-to [-1 1]
                :when (not= (pos tbl (+ i k) (+ j l)) (chr "."))]
               1)))

(defn moveable [tbl once?]
  (var count 0)
  (let [n (length tbl)
        m (length (tbl 0))]
    (forever
      (var changed false)
      (loop [i :range [0 n]
             j :range [0 m]]
        (when (and (not= (pos tbl i j) (chr "."))
                   (< (paper-rolls tbl i j) 5))
          (++ count)
          (put (tbl i) j (chr "#"))
          (set changed true)))
      (when (or once? (not changed))
        (break))
      (loop [i :range [0 n]
             j :range [0 m]
             :when (= (pos tbl i j) (chr "#"))]
        (put (tbl i) j (chr ".")))))
  count)

(print (moveable data true))    # also changes removable @s to #s
(print (moveable data false))   # removes all @s iteratively
