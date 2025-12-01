(def data
  (with [f (file/open "adv01.txt")]
        (map |(peg/match ~(* (+ (/ "L" :left) (/ "R" :right)) (number :d+)) $)
             (file/lines f))))

# 0x434C49434B ~ "CLICK"

(defn zeros [combination]
  (var part1 0)
  (var part2 0)
  (var dial 50)
  (each [dir n] combination
    (+= part2 (div (+ (if (= dir :left)
                        (if (= dial 0) 0 (- 100 dial))
                        dial)
                      n)
                   100))
    (set dial (mod (+ dial (if (= dir :left) (- n) n)) 100))
    (when (zero? dial)
      (++ part1)))
  [part1 part2])

(let [[p1 p2] (zeros data)]
  (printf "%d\n%d" p1 p2))
