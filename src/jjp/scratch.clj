(ns jjp.scratch
  (:require [clojure.string :as str]))

(defn nth-page
  "hello, this is a function"
  [source page-size page]
  (->> source
       (drop (* page-size page))
       (take page-size))
  )

(def c (seq "topleny"))

(str/join (shuffle c))

(defn tails [coll] (take-while seq (iterate rest coll)))
(defn inits [coll] (reductions conj [] coll))

(defn rotations [a-seq]
  (distinct (map concat (tails a-seq) (inits a-seq))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (apply concat (map (fn [x] (map cons (repeat (first x))
                                    (permutations (rest x))))
                       (rotations a-set)))))

(map str/join (permutations #{ \t \o \p \l \e \n \y }))

(+ 1 (* 2 3))

(> (+ 1 (* 2 3))
   (* 2 (+ 1 3)))

(def numbers [1 2 3 4 5 6 7 8 9 10])

(apply + numbers)

[127 0x7F 0177 32r3V 2r01111111]

[127 0x7F 0177 32r3V 2r01111111]

[127 0x7F 0177 32r3V 2r01111111]

[127 127 127 127 127]

(def yucky-pi 22/7)

(print (str/join [1 \a \b \n \newline \a \tab \b \c]))
(if () true false)

(def y)
y

(def make-set
  (fn [x y]
    (println "Making a set")
    #{x y}))

(make-set 1 2)

(defn make-set
  "Takes two values and makes a set from them."
  [x y]
  (println "Making a set")
  #{x y})

(make-set 1 2)
(defn make-set
  ([x] #{x})
  ([x y] #{x y}))

(make-set 1 5)

(quote `(1 2 3))

(str 1 '(2 3))
(str 1  [2 3])
