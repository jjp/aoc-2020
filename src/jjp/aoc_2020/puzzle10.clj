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

;;;;;; LI part 1
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


(defn cv [set]
  #_ (count) (for [x [(first set)]
                y set
                :when (and (not= x y)
                           (<= (Math/abs (- x y)) 3))]
            [x y]
            ))


(defn with-bounds [input]
  (concat [0] input [(+ 3 (last input))] )
  )
(def sets-1 (partition 4 1 (with-bounds demo-1-input)))
(def sets-2 (partition 4 1 (with-bounds demo-2-input)))
(def sets-real (partition 4 1 (with-bounds real-input)))

(with-bounds demo-1-input)
;; => (0 1 4 5 6 7 10 11 12 15 16 19 22)
(map cv sets-1)

(apply + (map count (filter #(> (count %) 1) (map cv sets-1))))
;; => 7
(apply + (map count (filter #(> (count %) 1) (map cv sets-2))))
;; => 39

(apply + (map count (filter #(> (count %) 1) (map cv sets-real))))

(map cv sets-1)
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
(map cv sets-2)
;; => (([0 1] [0 2] [0 3])
;;     ([1 2] [1 3] [1 4])
;;     ([2 3] [2 4])
;;     ([3 4])
;;     ([4 7])
;;     ([7 8] [7 9] [7 10])
;;     ([8 9] [8 10] [8 11])
;;     ([9 10] [9 11])
;;     ([10 11])
;;     ([11 14])
;;     ([14 17])
;;     ([17 18] [17 19] [17 20])
;;     ([18 19] [18 20])
;;     ([19 20])
;;     ([20 23])
;;     ([23 24] [23 25])
;;     ([24 25])
;;     ([25 28])
;;     ([28 31])
;;     ([31 32] [31 33] [31 34])
;;     ([32 33] [32 34] [32 35])
;;     ([33 34] [33 35])
;;     ([34 35])
;;     ([35 38])
;;     ([38 39])
;;     ([39 42])
;;     ([42 45])
;;     ([45 46] [45 47] [45 48])
;;     ([46 47] [46 48] [46 49])
;;     ([47 48] [47 49]))
