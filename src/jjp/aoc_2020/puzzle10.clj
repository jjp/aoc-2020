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


(defn cv [set]
  (count (for [x [(first set)]
               y set
               :when (and (not= x y)
                          (<= (Math/abs (- x y)) 3))]
           [x y]
           )))


(def sets-1 (partition 4 1 (concat [0] demo-1-input [(+ 3 (last demo-1-input))] )))
(def sets-2 (partition 4 1 (concat [0] demo-2-input [(+ 3 (last demo-2-input))] )))

;; => ((1 4 5 6)
;;     (4 5 6 7)
;;     (5 6 7 10)
;;     (6 7 10 11)
;;     (7 10 11 12)
;;     (10 11 12 15)
;;     (11 12 15 16)
;;     (12 15 16 19))
(let [sets sets-1 ]
  (apply + (map cv sets))
  )

sets-1
(apply + (map cv sets-2))
;; => (([0 1])
;;     ([1 4])
;;     ([4 5] [4 6] [4 7])
;;     ([5 6] [5 7])
;;     ([6 7])
;;     ([7 10])
;;     ([10 11] [10 12])
;;     ([11 12])
;;     ([12 15])
;;     ([15 16]))

;; => (([0 1])
;;     ([1 4])
;;     ([4 5] [4 6] [4 7])
;;     ([5 6] [5 7])
;;     ([6 7])
;;     ([7 10])
;;     ([10 11] [10 12])
;;     ([11 12])
;;     ([12 15])
;;     ([15 16]))


(defn part1 [input ]
  (->> (conj input 0 (+ 3 (apply max input)))
       sort
       (partition 2 1)
       (map #(- (second %) (first %)))
       frequencies
       vals
       (apply *)))
(part1 real-input)
;; => 2738
