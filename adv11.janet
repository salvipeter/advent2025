(def data
  (with [f (file/open "adv11.txt")]
        (->> (file/lines f)
             (map |(peg/match ~(* (<- :a+) ":" (group (some (* " " (<- :a+))))) $))
             from-pairs)))

(defn count-paths [connections from to visiting]
  (def cache @{})
  (defn aux [node to-visit]
    (def index [node ;to-visit])
    (cond
      (in cache index) (cache index)
      (= node to) (if (empty? to-visit) 1 0)
      (let [to-visit1 (filter |(not= $ node) to-visit)
            result (sum (map |(aux $ to-visit1) (connections node)))]
        (put cache index result)
        result)))
  (aux from visiting))

(print (count-paths data "you" "out" []))
(print (count-paths data "svr" "out" ["dac" "fft"]))
