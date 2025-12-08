(def data
  (with [f (file/open "adv08.txt")]
        (map |(peg/match ~(* (number :d+) "," (number :d+) "," (number :d+)) $)
             (file/lines f))))

(defn squared-distance [a b]
  (sum (map |(math/pow (- $0 $1) 2) a b)))

(defn pair-dists [points]
  (seq [[i a] :pairs points
        [j b] :pairs points
        :when (< i j)]
       [(squared-distance a b) i j]))

(defn update-groups
  "Updates `groups` of the form `@[@[1 3 7] @[4 5]]` by connecting `i` and `j`.
   Returns the modified array."
  [groups i j]
  (let [a (find-index |(has-value? $ i) groups)
        b (find-index |(has-value? $ j) groups)]
    (cond
      (and a b) (unless (= a b)
                  (array/concat (groups a) (groups b))
                  (array/remove groups b))
      a (array/push (groups a) j)
      b (array/push (groups b) i)
      (array/push groups @[i j])))
  groups)

(defn connect-pairs [connections]
  (reduce (fn [acc [_ i j]] (update-groups acc i j)) @[] connections))

(defn last-pair [n connections]
  (var groups @[])
  (some (fn [[_ i j]]
          (update-groups groups i j)
          (when (and (= (length groups) 1)
                     (= (length (groups 0)) n))
            [i j]))
        connections))

(let [sorted-pairs (sort (pair-dists data))]
  (print (->> (take 1000 sorted-pairs) (connect-pairs)
              (map length) (sort) (reverse) (take 3) (product)))
  (print (product (map |((data $) 0) (last-pair (length data) sorted-pairs)))))
