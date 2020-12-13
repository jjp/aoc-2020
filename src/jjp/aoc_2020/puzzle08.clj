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
    (into {} (map vec (partition 2 (interleave index instructions))))
    ;; (map vec (partition 2 (interleave index instructions)))
    ))

(map #(str/split % #" " ) demo-input)

(def program (compile (map #(str/split % #" " ) demo-input)))

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
    (if (and (contains? seen adr)
             (contains? program adr))
      [stack acc]
      (let [[op arg] (get program adr)
            seen (conj seen adr)
            stack (conj stack adr)
            [acc adr] (case op
                        "nop" [acc (inc adr)]
                        "jmp" [acc (+ adr arg)]
                        "acc" [(+ acc arg) (inc adr)]
                        )
            ]
        (recur acc seen stack adr)
        )
      )
  ))

(last (validate-last-accum (compile (map #(str/split % #" " ) demo-input)) ))
;; => 5

(last (validate-last-accum (compile (map #(str/split % #" " ) real-input))))
;; => 1553

(def orig-real-program (compile (map #(str/split % #" " ) real-input)))
(let [[stack acc] (validate-last-accum orig-real-program)]
  [(last stack) acc])
;; => [325 1553]
;; (def fixed-real-program (assoc orig-real-program 325 ["nop" 0]))
(let [[stack acc] (validate-last-accum fixed-real-program)]
  [(last stack) acc])
;; => [384 1592]
;; => [[0
;;      1
;;      2
;;      3
;;      4
;;      215
;;      216
;;      217
;;      165
;;      166
;;      167
;;      168
;;      452
;;      138
;;      139
;;      140
;;      141
;;      142
;;      70
;;      71
;;      72
;;      73
;;      74
;;      460
;;      515
;;      516
;;      517
;;      518
;;      358
;;      359
;;      305
;;      306
;;      307
;;      308
;;      145
;;      146
;;      147
;;      496
;;      407
;;      408
;;      208
;;      396
;;      397
;;      398
;;      278
;;      482
;;      483
;;      484
;;      335
;;      336
;;      337
;;      338
;;      192
;;      256
;;      257
;;      258
;;      259
;;      260
;;      202
;;      203
;;      117
;;      118
;;      119
;;      120
;;      121
;;      199
;;      200
;;      290
;;      291
;;      292
;;      293
;;      394
;;      504
;;      505
;;      62
;;      63
;;      64
;;      65
;;      66
;;      14
;;      125
;;      529
;;      530
;;      531
;;      532
;;      533
;;      231
;;      232
;;      233
;;      18
;;      19
;;      20
;;      98
;;      99
;;      100
;;      101
;;      102
;;      316
;;      317
;;      318
;;      128
;;      129
;;      36
;;      179
;;      180
;;      181
;;      182
;;      349
;;      402
;;      403
;;      475
;;      476
;;      477
;;      478
;;      479
;;      374
;;      375
;;      376
;;      377
;;      378
;;      586
;;      587
;;      111
;;      112
;;      113
;;      114
;;      115
;;      454
;;      455
;;      456
;;      457
;;      50
;;      51
;;      239
;;      240
;;      188
;;      472
;;      59
;;      354
;;      355
;;      356
;;      488
;;      489
;;      490
;;      368
;;      369
;;      418
;;      419
;;      420
;;      421
;;      54
;;      55
;;      56
;;      57
;;      557
;;      558
;;      559
;;      560
;;      561
;;      83
;;      84
;;      85
;;      195
;;      196
;;      197
;;      222
;;      223
;;      131
;;      132
;;      133
;;      134
;;      372
;;      427
;;      428
;;      429
;;      430
;;      341
;;      342
;;      343
;;      344
;;      576
;;      577
;;      578
;;      579
;;      580
;;      314
;;      380
;;      162
;;      163
;;      322
;;      323
;;      324
;;      325]
;;     1553]
