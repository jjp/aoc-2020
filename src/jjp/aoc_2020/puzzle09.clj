(ns jjp.aoc-2020.puzzle09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def demo-input (map #(Long/parseLong %) (str/split-lines demo-input)))

(def real-input
  (map #(Long/parseLong %)(str/split-lines (slurp (io/reader (io/resource "puzzle_input_09.txt")))))
  )

(for [x '(1 2)
      y '(1 2)
      :when (= 2 (+ x y))]
  [x y]
  )
(defn valid? [[sum & rest]]
  (let [pairs (for [x rest
                    y rest
                    :when (= sum (+ x y))]
                [x y]
                )]
    [sum (not (empty? pairs))]
    ))

(valid? '(62 40 47 25 15 20))
(valid? '(127 182 150 117 102 95))
(filter #(false? (last %))(map valid? (map reverse (partition 6 1 demo-input ))))
;; => ([127 false])
;; => ([40 true]
;;     [62 true]
;;     [55 true]
;;     [65 true]
;;     [95 true]
;;     [102 true]
;;     [117 true]
;;     [150 true]
;;     [182 true]
;;     [127 false]
;;     [219 true]
;;     [299 true]
;;     [277 true]
;;     [309 true]
;;     [576 true])

(filter #(false? (last %))(map valid? (map reverse (partition 26 1 real-input ))))
;; => ([393911906 false])
