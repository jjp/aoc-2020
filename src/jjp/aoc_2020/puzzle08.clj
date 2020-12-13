(ns jjp.aoc-2020.puzzle08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hashp.core]))

(def real-input
  (line-seq (io/reader (io/resource "puzzle_input_08.txt")))
  )

(def demo-input
  (line-seq (io/reader (io/resource "puzzle_input_08_demo.txt")))
  )


(vec (map #(str/split % #" " ) demo-input))
;; => [["nop" "+0"]
;;     ["acc" "+1"]
;;     ["jmp" "+4"]
;;     ["acc" "+3"]
;;     ["jmp" "-3"]
;;     ["acc" "-99"]
;;     ["acc" "+1"]
;;     ["jmp" "-4"]
;;     ["acc" "+6"]]

(defn compile [instructions]
  (let [instructions  (map (fn [[i a]] [i (Long/parseLong a)]) instructions)
        index (range 0 (count instructions))
        ]
    ;; (into {} (map vec (partition 2 (interleave index instructions))))
    (into [] (map vec (partition 2 (interleave index instructions))))
    ))

(map #(str/split % #" " ) demo-input)

(def program (compile (map #(str/split % #" " ) demo-input)))
program
;; => ([0 ["nop" 0]]
;;     [1 ["acc" 1]]
;;     [2 ["jmp" 4]]
;;     [3 ["acc" 3]]
;;     [4 ["jmp" -3]]
;;     [5 ["acc" -99]]
;;     [6 ["acc" 1]]
;;     [7 ["jmp" -4]]
;;     [8 ["acc" 6]])
;; => {0 ["nop" 0],
;;     7 ["jmp" -4],
;;     1 ["acc" 1],
;;     4 ["jmp" -3],
;;     6 ["acc" 1],
;;     3 ["acc" 3],
;;     2 ["jmp" 4],
;;     5 ["acc" -99],
;;     8 ["acc" 6]}

(defn validate-last-accum [program]
  (loop [acc 0
         seen #{}
         stack []
         adr 0]
    (if (or  (contains? seen adr)
             (nil? adr)
             (not (<= 0 adr (dec (count program))))
             )
      [stack acc]
      (let [ [_ [op arg]] (nth program adr)
            seen (conj seen adr)
            stack (conj stack adr)
            [acc adr] (case op
                        "nop" [acc (inc adr)]
                        "jmp" [acc (+ adr arg)]
                        "acc" [(+ acc arg) (inc adr)]
                        [acc nil]
                        )
            ]
        (recur acc seen stack adr)
        )
      )
  ))

(def demo-program (compile (map #(str/split % #" " ) demo-input)))

(count demo-program)
(nth demo-program 0)
(let [[stack acc](validate-last-accum demo-program )]
   [(last stack) acc] )
;; => [4 5]
(def fixed-demo-program (assoc demo-program 7 [7 ["nop" -4]]))
(let [[stack acc](validate-last-accum fixed-demo-program )]
  [(last stack) acc] )


(last (validate-last-accum (compile (map #(str/split % #" " ) real-input))))
;; => 1553

(def orig-real-program (compile (map #(str/split % #" " ) real-input)))
(let [[stack acc] (validate-last-accum orig-real-program)]
  [(last stack) acc])
;; => [325 1553]

(get orig-real-program 325)
;; => ["jmp" 127]
(get orig-real-program (+ 127 325))
;; => ["jmp" -314]

(def fixed-real-program (assoc orig-real-program 452 ["nop" 0]))
(let [[stack acc] (validate-last-accum fixed-real-program)]
  [(last stack) acc])
;; => [384 1592]
