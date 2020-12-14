(ns jjp.aoc-2020.puzzle10
  (:require [jjp.aoc-2020.util :as util]))

(def demo-1-input
  (sort [ 16 10 15  5   1   11   7   19   6   12   4 ])
  )

(def demo-2-input
  (sort [28
         33
         18
         42
         31
         14
         46
         20
         48
         47
         24
         23
         49
         45
         19
         38
         39
         11
         1
         32
         25
         35
         8
         17
         7
         9
         4
         2
         34
         10
         3]))

(def real-input (sort (util/num-resource-seq 10)))


(frequencies (map #(Math/abs (apply - %)) (partition 2 1 demo-1-input)))
;; => (3 1 1 1 3 1 1 3 1 3)
()
(frequencies (map #(Math/abs (apply - %)) (partition 2 1 demo-2-input)))
;; => {1 21, 3 9}

(apply * (map inc (vals (frequencies (map #(Math/abs (apply - %)) (partition 2 1 demo-2-input))))))
;; => (21 9)


(apply * (map inc (vals (frequencies (map #(Math/abs (apply - %)) (partition 2 1 real-input))))))
;; => 2738
