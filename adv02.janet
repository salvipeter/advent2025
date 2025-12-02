(def data
  (peg/match ~(some (group (* (number :d+) "-" (number :d+) (any ","))))
              (slurp "adv02.txt")))

(defn part1 [n]
  (let [s (string n)
        k (length s)
        m (div k 2)]
    (and (even? k)
         (= (string/slice s 0 m)
            (string/slice s m)))))

(defn part2 [n]
  (let [s (string n)
        k (length s)]
    (some |(and (zero? (% k $))
                (peg/match ~(* 0 (some ,(string/slice s 0 $)) -1)
                            (string/slice s $)))
          (range 1 (inc (div k 2))))))

(defn fakes [pred ranges]
  (sum (seq [[a b] :in ranges n :range [a b] :when (pred n)] n)))

(print (fakes part1 data))
(print (fakes part2 data))
