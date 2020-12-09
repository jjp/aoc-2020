(ns jjp.aoc-2020.puzzle06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def real-input
  (slurp (io/reader (io/resource "puzzle_input_06.txt")))
  )

(def demo-input "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn parse-group [entry]
  (into [] (map (comp vec next)) (re-seq #"(\w)" entry)))

(defn yesses [answers]
  (set (flatten answers)))

(->>
 (str/split demo-input #"\R\R")
 (map parse-group)
 (map yesses)
 (map count)
 (apply +)
 )
;; => 11
;; => ([["a"] ["b"] ["c"]]
;;     [["a"] ["b"] ["c"]]
;;     [["a"] ["b"] ["a"] ["c"]]
;;     [["a"] ["a"] ["a"] ["a"]]
;;     [["b"]])
;; => (("a" "b" "c") ("a" "b" "c") ("a" "b" "a" "c") ("a" "a" "a" "a") ("b"))
;; => 11

;; => (#{"a" "b" "c"} #{"a" "b" "c"} #{"a" "b" "c"} #{"a"} #{"b"})

;; => (("abc") ("a" "b" "c") ("ab" "ac") ("a" "a" "a" "a") ("b"))

;; => ("abc" "a" "b" "c" "ab" "ac" "a" "a" "a" "a" "b")
;; => ([["abc"]]
;;     [["a"] ["b"] ["c"]]
;;     [["ab"] ["ac"]]
;;     [["a"] ["a"] ["a"] ["a"]]
;;     [["b"]])
;; => ["abc" "a\nb\nc" "ab\nac" "a\na\na\na" "b"]

(->>
 (str/split real-input #"\R\R")
 (map parse-group)
 (map yesses)
 (map count)
 (apply +)
 )
;; => 6590

;; part2

(defn parse-group2 [entry]
  (into [] (->> (str/split entry #"\R")
                (map seq)
                (map set)))
  )

(apply set/intersection [#{1} #{1}])

(->> (map #(apply set/intersection %)
         (->>
          (str/split demo-input #"\R\R")
          (map parse-group2)
          ))
    (map count)
    (apply +))
;; => 6
;; => ([#{\a \b \c}]
;;     [#{\a} #{\b} #{\c}]
;;     [#{\a \b} #{\a \c}]
;;     [#{\a} #{\a} #{\a} #{\a}]
;;     [#{\b}])

(->> (map #(apply set/intersection %)
          (->>
           (str/split real-input #"\R\R")
           (map parse-group2)
           ))
     (map count)
     (apply +))
;; => 3288
