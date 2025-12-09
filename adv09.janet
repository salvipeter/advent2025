(def data
  (with [f (file/open "adv09.txt")]
        (map |(peg/match ~(* (number :d+) "," (number :d+)) $)
             (file/lines f))))

(defn area [[px py] [qx qy]]
  (* (inc (math/abs (- qx px)))
     (inc (math/abs (- qy py)))))

(defn pair-areas [points]
  (seq [[i a] :pairs points
        [j b] :pairs points
        :when (< i j)]
       [(area a b) a b]))

(defn segments [points]
  (map tuple points (tuple ;(drop 1 points) (first points))))

(defn intersects-inside?
  "Does the rectangle (px,py)x(qx,qy) intersect
   the line segment (ax,ay)-(bx,by) in its interior?"
  [[[px py] [qx qy]] [[ax ay] [bx by]]]
  (or (and (= ax bx)
           # vertical segment
           (< (min px qx) ax (max px qx))
           (> (max ay by) (min py qy))
           (> (max py qy) (min ay by)))
      (and (= ay by)
           # horizontal segment
           (< (min py qy) ay (max py qy))
           (> (max ax bx) (min px qx))
           (> (max px qx) (min ax bx)))))

(defn red-green-rectangle? [rect segs]
  (not (some |(intersects-inside? rect $) segs)))

(let [sorted (reverse (sort (pair-areas data)))
      segs (segments data)]
  (print (first (first sorted)))
  (print (first (find |(red-green-rectangle? (drop 1 $) segs) sorted))))
