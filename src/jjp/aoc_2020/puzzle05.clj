(ns jjp.aoc-2020.puzzle05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def real-input
  (slurp (io/reader (io/resource "puzzle_input_05.txt")))
  )


(defn row [pass]
  (loop [rmap pass
         block (range 0 128)
         ]
    (let [size (count block)
          dir (first rmap)
          block (case dir
                  \F (first (partition (int (/ size 2)) block))
                  \B (second (partition (int (/ size 2)) block))
                  block) ]
      (if-not dir
        (first block)
        (recur (rest rmap) block )
        ))))

(defn col [pass]
  (loop [rmap pass
         block (range 0 8)
         ]
    (let [size (count block)
          dir (first rmap)
          block (case dir
                  \L (first (partition (int (/ size 2)) block))
                  \R (second (partition (int (/ size 2)) block))
                  block) ]
      (if-not dir
        (first block)
        (recur (rest rmap) block )
        ))))

;; BFFFBBFRRR: row 70, column 7, seat ID 567.
;; FFFBBBFRRR: row 14, column 7, seat ID 119.
;; BBFFBBFRLL: row 102, column 4, seat ID 820.

(defn pass-id [pass]
  (let [[_ r c] (re-find #"^([BF]{7}?)([RL]{3}?)" pass)]
    (+ (* 8 (row r))
       (col c))
    ))

(apply max [ 1 2 3])
(apply max (map pass-id ["BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL" ]))
(apply max (map pass-id (str/split-lines real-input)))
;; => 951

; part 2
(time (let [sorted (sort (map pass-id (str/split-lines real-input)))
       fs (first sorted)
       ls (last sorted)
       seated (set sorted)
       all-seats (set (range fs (- ls fs)))]
   (set/difference all-seats seated)
   ))
;; => #{653}
