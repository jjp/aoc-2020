(ns jjp.aoc-2020.puzzle03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input
  (->
   "..##.........##.........##.........##.........##.........##.......
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
   (str/split #"\n"))
  )

(str/join (second (partition 69 (str/join demo-input))))

(def input
  (slurp (io/reader (io/resource "puzzle_input_03.txt")))
  )

(str/join demo-input)
(count (first demo-input))
(defn count-trees [[x y] rows]
  (let [l (count (first rows))
        gap (+ l x)
        full-string (rest (str/join rows)) ;; I think we need to drop first char due to starting pos
        part  (doall (partition gap full-string))
        jumps (doall (map last part))
        c (count (filter #(= \# %) jumps))
        ]
    ;; c
    {:c c :g gap :j jumps}
    )
  )

(count-trees [3 1] demo-input)
;; => 7

(def r (count-trees [3 1] input))

;; help from lambdaisland - map to vector and then to map to true / false
;; I originally misunderstood the map model and neglected that it went
;; infinitely to the right, I just assumed I wrapped

(def demo-input-l
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def my-map
  (mapv (fn [row]
          (mapv {\# true \. false} row))
        (str/split demo-input-l #"\n")))

(defn tree? [m x y]
  (let [width (count (first m))]
    (get-in m [y (mod x width)])))

(def pos [my-map 0 0 0])

(defn make-sled [[slope-x slope-y]]
  (fn [[my-map x y trees]]
    (let [x (+ x slope-x)
          y (+ y slope-y)
          tree? (tree? my-map x y)]
      (cond
        (nil? tree?)
        (reduced trees)

        (true? tree?)
        [my-map x y (inc trees)]

        :else
        [my-map x y trees]))))

(-> pos
    sled
    sled
    sled
    sled
    sled
    sled
    sled
    sled
    sled
    sled
    sled
    )
;; => #<Reduced@1025492f: 7>

(defn input->map [input]
  (mapv (fn [row]
          (mapv {\# true \. false} row))
        (str/split input #"\n"))
  )

@(first (drop-while
  (complement reduced?)
  (iterate (make-sled [3 1]) [(input->map input) 0 0 0])))
;; => 276

(def slopes
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

(defn count-trees [slope]
  @(first (drop-while
           (complement reduced?)
           (iterate (make-sled slope) [(input->map input) 0 0 0])))
  )

(time (reduce *
         (map #(count-trees %)
              slopes)
         ))
;; => 7812180000
