(ns jjp.aoc-2020.puzzle11
  (:require [jjp.aoc-2020.util :as util]
            [clojure.string :as str]))

(def demo-input (str/split "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL" #"\n"))

(defn parse-input [input]
  (mapv (fn [row]
          (mapv {\L :emp \. :flo \# :occ} row))
        input))

(def real-input (str/split (slurp (util/puzzle-input 11)) #"\n"))

(def compass [
              [-1 -1] [-1 0] [-1 1]
              [0 -1] [0 1]
              [1 -1] [1 0] [1 1]
              ])


(defn count-surrounding [seat-type b r c]
  (let [seats-to-check (mapv (fn [[x y]] [(+ x r) (+ y c)]) compass)]
    (count (filter #(= seat-type (get-in b %)) seats-to-check ))
    )
  )

(defn next-empty [b r c]
  (if (= (count-surrounding :occ b r c) 0)
    :occ
    :emp)
  )

(defn next-occupied [b r c]
  (if (>= (count-surrounding :occ b r c) 4)
    :emp
    :occ)
  )

(defn next-seat-state [b r c]
  (case (get-in b [r c])
    :flo :flo
    :emp (next-empty b r c)
    :occ (next-occupied b r c))
)

(defn next-row-state [b r]
  (let [cols (count (first b))
        next-row (mapv (partial next-seat-state b r) (range cols))]
    next-row
    )
  )

(defn take-turn [board]
  (let [cols (count (first board))
        rows (count board)
        next-board (mapv (partial next-row-state board) (range rows))
        ]
    next-board
    ))

(first (parse-input demo-input))
;; => [:empty :floor :empty :empty :floor :empty :empty :floor :empty :empty]
(first (take-turn (parse-input demo-input)))
;; => [:occupied
;;     :floor
;;     :occupied
;;     :occupied
;;     :floor
;;     :occupied
;;     :occupied
;;     :floor
;;     :occupied
;;     :occupied]

(def b (parse-input demo-input))

(defn display [input]
  (mapv (fn [row]
          (str/join (mapv {:emp \L :flo \. :occ \# } row)))
        input))

(defn play [input]
  (let [board input]
    (loop [board board
           next-board (take-turn board)]
      (if (= board next-board)
        board
        (recur next-board (take-turn next-board)))
      ))
  )


(->> (play (parse-input demo-input))
     flatten
     (filter #(= :occupied %))
     count
     )
;; => 2265
