(def data
  (with [f (file/open "adv06.txt")]
        (seq (line :in (file/lines f))
             (string/slice line 0 -2))))

(def parsed
  (map |(peg/match ~(some (+ :s (number :d+) (/ "+" :sum) (/ "*" :product))) $)
       data))

(let [n (length parsed)
      m (length (parsed 0))]
  (print (sum (seq [i :range [0 m]]
                   ((if (= ((parsed (dec n)) i) :sum) + *)
                    ;(map |($ i) (take (dec n) parsed)))))))

(let [n (length data)
      m (length (data 0))]
  (var result 0)
  (var nums @[])
  (loop [i :down-to [(dec m) 0]]
    (array/push nums 0)
    (var found false)
    (for j 0 n
      (let [c ((data j) i)]
        (case c
          (chr " ") nil
          (chr "+") (do (+= result (+ ;nums))
                        (set nums @[]))
          (chr "*") (do (+= result (* ;nums))
                        (set nums @[]))
          (let [x (array/pop nums)]
            (array/push nums (+ (* x 10) (- c (chr "0"))))
            (set found true)))))
    (unless found
      (array/pop nums)))
  (print result))
