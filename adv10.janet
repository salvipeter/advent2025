(def data
  (with [f (file/open "adv10.txt")]
        (map |(peg/match ~(* "[" (group (some (+ (/ "." false) (/ "#" true)))) "] "
                             (group (some (group (* "(" (some (* (number :d+) (? ","))) ") "))))
                             "{" (group (some (* (number :d+) (? ",")))) "}") $)
             (file/lines f))))

(defn subsets [lst]
  (if (empty? lst)
    [[]]
    (let [rest (subsets (drop 1 lst))]
      (tuple/join rest (map |(tuple (lst 0) ;$) rest)))))

(defn good-subset? [lst goal]
  (all (fn [i] (= (odd? (count |(index-of i $) lst)) (goal i)))
       (range 0 (length goal))))

(defn minimal-pushes-lights [buttons goal]
  (let [sets (sorted-by length (subsets buttons))
        x (find |(good-subset? $ goal) sets)]
    (and x (length x))))

(defn prune-buttons [buttons goal]
  (filter (fn [b] (all |(> (goal $) 0) b)) buttons))

(defn fixed-first [buttons goal]
  (let [x (find (fn [i]
                  (and (> (goal i) 0)
                       (= (count |(index-of i $) buttons) 1)))
                (range (length goal)))
        b (and x (find |(index-of x $) buttons))]
    (if x
      (array b ;(filter |(not= b $) buttons))
      buttons)))

(defn minimal-pushes-joltage [buttons goal]
  (defn aux [n least buttons goal]
    (cond
      (and least (> n least)) false
      (all |(= $ 0) goal) n
      (empty? buttons) false
      (let [b (buttons 0)
            rest (drop 1 buttons)
            limit (min ;(map |(goal $) b))
            only-here (filter (fn [i] (not (some |(index-of i $) rest))) b)
            fixed (map |(goal $) only-here)]
        (var best least)
        (if (or (and (> (length fixed) 1)
                     (find |(not= $ (fixed 0)) (drop 1 fixed)))
                (and (not (empty? fixed))
                     (> (fixed 0) limit))
                (some (fn [i] (and (> (goal i) 0)
                                   (not (some |(index-of i $) buttons))))
                      (range (length goal))))
          false
          (let [from (if (empty? fixed) 0 (fixed 0))
                to (if (empty? fixed) limit (fixed 0))
                better-rest (if (= to limit)
                              (fixed-first (prune-buttons rest goal) goal)
                              (fixed-first rest goal))]
            (for i from (inc to)
              (var new-goal (array ;goal))
              (each j b
                (put new-goal j (- (goal j) i)))
              (let [res (aux (+ n i) best better-rest new-goal)]
                (when res
                  (set best res))))))
        best)))
  (aux 0 false (reverse (sorted-by length buttons)) goal))

(print (sum (map (fn [[blinkenlights buttons _]]
                   (minimal-pushes-lights buttons blinkenlights))
                 data)))
(print (sum (map (fn [[_ buttons joltages]]
                   (let [r (minimal-pushes-joltage buttons joltages)]
                     # (pp [joltages :result r])
                     r))
                 data)))
