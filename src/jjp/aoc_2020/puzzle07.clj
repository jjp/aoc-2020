(ns jjp.aoc-2020.puzzle07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def real-input
  (slurp (io/reader (io/resource "puzzle_input_07.txt")))
  )

(def demo-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
)

(defn parse-content-rules [entry]
  (set/map-invert (into {}
         (map (comp vec next)
              (re-seq #"(\d+) (\w+ \w+) bag" entry) )
         ))
  )


(def x (comp vec next))

(map x) (re-seq #"(\d+) (\w+ \w+) bag" "1 bright white bag")

(defn parse-rule [entry]
  (let [[_ bag contents-raw] (re-find #"(\w+ \w+) bags contain (.*)$" entry)
        content-rules (into {} (map parse-content-rules (str/split contents-raw #", "))) ]
    {bag content-rules}
    )
  )

(def demo-rules (into {} (->> (str/split-lines demo-input)
                              (map parse-rule)
                              )))

(defn make-rules [input] (into {} (->> (str/split-lines input)
                                  (map parse-rule)
                                  )))

(count (filter #(contains? % "shiny gold") (map #(set (keys %)) (vals (make-rules demo-input)))))
;; => 2

(count (filter #(contains? % "shiny gold") (map #(set (keys %)) (vals (make-rules real-input)))))
;; => 7


(defn sets-of-contents [m]
  (map #(set (keys %)) (vals m))
  )

(defn can-hold? [s m]
  (let [x (reduce into #{} (sets-of-contents (select-keys m s)))
        deep (set/union s x)
        ]
    (prn deep)
    (contains? deep "shiny gold")
    )
  )

(def r (make-rules demo-input))

(vals (select-keys r #{"shiny gold"}))
;; (filter #(contains % "shiny gold")(sets-of-contents (select-keys top-rules %)))

(let [top-rules (make-rules real-input)
      content-sets (sets-of-contents top-rules)
      count-top (count (filter #(can-hold? % top-rules) content-sets ))
      ]
  count-top)

;; => (#{"shiny gold" "faded blue"}
;;     #{"muted yellow" "bright white"}
;;     #{}
;;     #{"muted yellow" "bright white"}
;;     #{"shiny gold"}
;;     #{"vibrant plum" "dark olive"}
;;     #{}
;;     #{"dotted black" "faded blue"}
;;     #{"dotted black" "faded blue"})
;; => {"muted yellow" {"shiny gold" "2", "faded blue" "9"},
;;     "light red" {"bright white" "1", "muted yellow" "2"},
;;     "dotted black" {},
;;     "dark orange" {"bright white" "3", "muted yellow" "4"},
;;     "bright white" {"shiny gold" "1"},
;;     "shiny gold" {"dark olive" "1", "vibrant plum" "2"},
;;     "faded blue" {},
;;     "vibrant plum" {"faded blue" "5", "dotted black" "6"},
;;     "dark olive" {"faded blue" "3", "dotted black" "4"}}

;; => {"muted yellow" {"shiny gold" "2", "faded blue" "9"},
;;     "light red" {"bright white" "1", "muted yellow" "2"},
;;     "dotted black" {},
;;     "dark orange" {"bright white" "3", "muted yellow" "4"},
;;     "bright white" {"shiny gold" "1"},
;;     "shiny gold" {"dark olive" "1", "vibrant plum" "2"},
;;     "faded blue" {},
;;     "vibrant plum" {"faded blue" "5", "dotted black" "6"},
;;     "dark olive" {"faded blue" "3", "dotted black" "4"}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LI

(def real-input
  (line-seq (io/reader (io/resource "puzzle_input_07.txt")))
  )

(def demo-input
  (line-seq (io/reader (io/resource "puzzle_input_07_demo.txt")))
  )

(defn parse-entry [s]
  (let [[bag & deps] (str/split s #"\s?(contain|,)\s?")
        color (re-find #"\w+ \w+" bag)]
    [color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)" )) deps)]
    ))

;; re-find returns match PLUS groups, use comp next pattern to drop the match
(comp next (partial re-find #"(\d+) (\w+ \w+)" ))

(map parse-entry demo-input)
;; => (["light red" (("1" "bright white") ("2" "muted yellow"))]
;;     ["dark orange" (("3" "bright white") ("4" "muted yellow"))]
;;     ["bright white" (("1" "shiny gold"))]
;;     ["muted yellow" (("2" "shiny gold") ("9" "faded blue"))]
;;     ["shiny gold" (("1" "dark olive") ("2" "vibrant plum"))]
;;     ["dark olive" (("3" "faded blue") ("4" "dotted black"))]
;;     ["vibrant plum" (("5" "faded blue") ("6" "dotted black"))]
;;     ["faded blue" ()]
;;     ["dotted black" ()])

(defn color-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m col conj bag))
                    m deps))
          {}
          entries))

(defn add-valid [result graph color]
  (into result (get graph color)))

(defn valid-outermost [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [res color]
                            (add-valid res graph color))
                          result result)]
      (if (= result result2)
        result
        (recur result2)))))

(count (valid-outermost (color-graph (map parse-entry demo-input)) "shiny gold"))
(count (valid-outermost (color-graph (map parse-entry real-input)) "shiny gold"))

;; part 2

(defn nesting-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m bag conj [(Long/parseLong num) col]))
                    m deps))
          {}
          entries))

(def graph (nesting-graph (map parse-entry real-input)))

(defn color-count [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce
       (fn [cnt [num color]]
         (+ cnt (* num (color-count graph color))))
       1
       entries)
      1)))

(dec (color-count graph "shiny gold"))
;; => 6683
