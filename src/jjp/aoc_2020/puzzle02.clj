(ns jjp.aoc-2020.puzzle02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            ))

(def demo-input [
            "1-3 a: abcde"
            "1-3 b: cdefg"
            "2-9 c: ccccccccc"
            ])


(def input (line-seq (io/reader (io/resource "puzzle_input_02.txt"))))

(defn validate-occurences?
  "validate that password has between min/max occurrences of ch"
  [[min max ch password]]
  (let [
        min (Long/parseLong min)
        max (Long/parseLong max)
        count (get-in (frequencies password) ch)
        valid? (and count (>= count min) (<= count max))]
    valid?
    )
  )

;; I mis-interpreted the rules - and added a condition that wasn't valid
;;
(defn ovalidate-positions?
  "validate that password exactly one occurrence of ch at either pos1 or pos2"
  [[p1 p2 ch password]]
  (let [
        p1 (Long/parseLong p1)
        p2 (Long/parseLong p2)
        count (count (re-seq (re-pattern ch) password))
        index (inc (or (str/index-of password ch) 0))
        valid? (and (= count 2) (or (= index p1)
                                    (= index p2)))

        ]
    {:ch ch :valid valid? :cnt count :idx index :p1 p1 :p2 p2 :pw password }
    )
  )

(defn validate-positions?
  "validate that password exactly one occurrence of ch at either pos1 or pos2"
  [[p1 p2 ch password]]
  (let [
        p1 (Long/parseLong p1)
        p2 (Long/parseLong p2)
        c1 (nth password (dec p1))
        c2 (nth password (dec p2))
        ch (nth ch 0)
        x [(= c1 ch)(= c2 ch)]
        valid? (= 1 (count (filter true? x)))
        ]
    {:valid valid?}
    )
  )


(count (filter identity (map #(validate-occurences? %)
     (map #(str/split % #":" )
          (map #(str/replace % #"(\d+)-(\d+) (\w): (\w)" "$1:$2:$3:$4") input)))))
;; => 536;; => 536

(count (filter identity
               (map #(validate-positions? %)
                    (map #(str/split % #":" )
                         (map #(str/replace % #"(\d+)-(\d+) (\w): (\w)" "$1:$2:$3:$4") input)))
               ))


(count (filter #(or (:valid %))(map #(validate-positions? %)
     (map #(str/split % #":" )
          (map #(str/replace % #"(\d+)-(\d+) (\w): (\w)" "$1:$2:$3:$4") input)))))
;; => 558
