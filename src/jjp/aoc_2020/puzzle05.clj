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

;; Lambda Island - realizes that BFFFBBR LRL are just binary numbers
(def chars {\F 0 \B 1 \L 0 \R 1})

(let [[row col] (partition-all 7 "BFFFBBFRRR")]
  #_ (Long/parseLong (apply str (map chars row)) 2)
  )

;; bit math using reduce - I need to read up on reduce approach
(defn bits->num [bits]
  (reduce #(+ (* %1 2) %2) 0 (map chars bits)))

(def seat-ids (map bits->num (str/split-lines real-input)))

(apply max seat-ids)
;; => 951

;; this uses partition-all to build a "sliding-window over seat-ids
;; then when we find a gap of 2 between l and h, return l+1
(some (fn [[l h]]
        (when (= h (+ 2 l))
          (inc l)))
      (partition-all 2 1 (sort seat-ids)))
;; => 653
