(def data
  (peg/match ~(* (group (some (group (* (number :d+) "-" (number :d+) (any "\n")))))
                 (group (some (* (number :d+) "\n"))))
              (slurp "adv05.txt")))

(defn in-range? [x]
  |(<= ($ 0) x ($ 1)))

(defn fresh [inventory]
  (count |(find (in-range? $) (inventory 0))
         (inventory 1)))

(defn overlaps? [[ax ay] [bx by]]
  (and (>= (inc ay) bx)
       (>= (inc by) ax)))

(defn merge-ranges [[ax ay] [bx by]]
  [(min ax bx) (max ay by)])

(defn range-length [[x y]] (- y x -1))

(defn combined-length [ranges]
  (if (empty? ranges)
    0
    (let [aux (fn [[current acc] range]
                (if (overlaps? current range)
                  [(merge-ranges current range) acc]
                  [current (array/push acc range)]))
          [merged rest] (reduce aux [(ranges 0) @[]] (drop 1 ranges))]
      (if (deep= merged (ranges 0))
        (+ (range-length merged) (combined-length rest))
        (combined-length (tuple merged ;rest))))))

(print (fresh data))
(print (combined-length (data 0)))
