(ns jjp.aoc-2020.puzzle02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn validate-positions?
  "validate that password exactly one occurrence of ch at either pos1 or pos 2"
  [[p1 p2 ch password]]
  (let [
        p1 (Long/parseLong p1)
        p2 (Long/parseLong p2)
        count (count (re-seq (re-pattern ch) password))
        index (inc (or (str/index-of password ch) 0))
        valid? (and (= count 1) (or (= index p1)
                                    (= index p2)))]
    {:ch ch :password password :valid valid? :index index :count count :p1 p1 :p2 p2 }
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


(filter #(= 1 (:count %))(map #(validate-positions? %)
     (map #(str/split % #":" )
          (map #(str/replace % #"(\d+)-(\d+) (\w): (\w)" "$1:$2:$3:$4") input))))
;; => ({:ch "k", :password "zbkt", :valid true, :index 3, :count 1, :p1 2, :p2 3}
;;     {:ch "k",
;;      :password "ccvmrkxl",
;;      :valid false,
;;      :index 6,
;;      :count 1,
;;      :p1 2,
;;      :p2 3}
;;     {:ch "g",
;;      :password "mvcfkrfcgppfjv",
;;      :valid false,
;;      :index 9,
;;      :count 1,
;;      :p1 5,
;;      :p2 8}
;;     {:ch "m",
;;      :password "ntqmdkhrrj",
;;      :valid false,
;;      :index 4,
;;      :count 1,
;;      :p1 2,
;;      :p2 10}
;;     {:ch "q",
;;      :password "qbhbcwmhph",
;;      :valid false,
;;      :index 1,
;;      :count 1,
;;      :p1 4,
;;      :p2 6}
;;     {:ch "v", :password "pgsjv", :valid true, :index 5, :count 1, :p1 3, :p2 5}
;;     {:ch "f", :password "pfcm", :valid true, :index 2, :count 1, :p1 1, :p2 2}
;;     {:ch "z", :password "lptzfpx", :valid false, :index 4, :count 1, :p1 2, :p2 6}
;;     {:ch "b", :password "bttm", :valid false, :index 1, :count 1, :p1 3, :p2 4}
;;     {:ch "n", :password "rnvx", :valid true, :index 2, :count 1, :p1 2, :p2 3}
;;     {:ch "d", :password "dmfp", :valid true, :index 1, :count 1, :p1 1, :p2 3}
;;     {:ch "x", :password "bclxbtq", :valid true, :index 4, :count 1, :p1 4, :p2 7}
;;     {:ch "f",
;;      :password "dfcztnztghj",
;;      :valid false,
;;      :index 2,
;;      :count 1,
;;      :p1 1,
;;      :p2 4}
;;     {:ch "x", :password "nkbvx", :valid true, :index 5, :count 1, :p1 4, :p2 5}
;;     {:ch "b", :password "bdhs", :valid true, :index 1, :count 1, :p1 1, :p2 2}
;;     {:ch "r",
;;      :password "bhlqqxszwnnstrff",
;;      :valid false,
;;      :index 14,
;;      :count 1,
;;      :p1 12,
;;      :p2 16}
;;     {:ch "f", :password "pxdrf", :valid true, :index 5, :count 1, :p1 3, :p2 5}
;;     {:ch "s", :password "pslhftrg", :valid true, :index 2, :count 1, :p1 2, :p2 6}
;;     {:ch "l",
;;      :password "jkrmtrnflxhj",
;;      :valid false,
;;      :index 9,
;;      :count 1,
;;      :p1 1,
;;      :p2 2}
;;     {:ch "p", :password "jqpkm", :valid false, :index 3, :count 1, :p1 4, :p2 5}
;;     {:ch "x", :password "gxhl", :valid true, :index 2, :count 1, :p1 2, :p2 4}
;;     {:ch "x", :password "sxrqkl", :valid false, :index 2, :count 1, :p1 4, :p2 6}
;;     {:ch "j", :password "jkwlt", :valid false, :index 1, :count 1, :p1 2, :p2 5}
;;     {:ch "m", :password "qmvdv", :valid true, :index 2, :count 1, :p1 2, :p2 3}
;;     {:ch "k",
;;      :password "tdqwklhlwncdz",
;;      :valid false,
;;      :index 5,
;;      :count 1,
;;      :p1 7,
;;      :p2 9}
;;     {:ch "q", :password "sqpjm", :valid true, :index 2, :count 1, :p1 2, :p2 5}
;;     {:ch "k",
;;      :password "vdqhfccdkjm",
;;      :valid false,
;;      :index 9,
;;      :count 1,
;;      :p1 2,
;;      :p2 8}
;;     {:ch "z", :password "tszh", :valid false, :index 3, :count 1, :p1 2, :p2 4}
;;     {:ch "h", :password "thzdq", :valid false, :index 2, :count 1, :p1 4, :p2 5}
;;     {:ch "z", :password "dkzb", :valid false, :index 3, :count 1, :p1 2, :p2 4}
;;     {:ch "t",
;;      :password "wqmtqzkln",
;;      :valid true,
;;      :index 4,
;;      :count 1,
;;      :p1 2,
;;      :p2 4}
;;     {:ch "k",
;;      :password "dpjrsxkhn",
;;      :valid false,
;;      :index 7,
;;      :count 1,
;;      :p1 2,
;;      :p2 5}
;;     {:ch "z", :password "bsrz", :valid false, :index 4, :count 1, :p1 2, :p2 3}
;;     {:ch "c",
;;      :password "jjtwmcgl",
;;      :valid false,
;;      :index 6,
;;      :count 1,
;;      :p1 3,
;;      :p2 4}
;;     {:ch "b", :password "bqkp", :valid false, :index 1, :count 1, :p1 2, :p2 3}
;;     {:ch "g", :password "zxgtfb", :valid false, :index 3, :count 1, :p1 4, :p2 6}
;;     {:ch "p", :password "vvpm", :valid true, :index 3, :count 1, :p1 3, :p2 4}
;;     {:ch "n", :password "rnbs", :valid false, :index 2, :count 1, :p1 3, :p2 4}
;;     {:ch "q", :password "zcgfq", :valid false, :index 5, :count 1, :p1 1, :p2 2}
;;     {:ch "n", :password "ktskn", :valid true, :index 5, :count 1, :p1 4, :p2 5}
;;     {:ch "g",
;;      :password "cqnrtxhkgzrmxfbp",
;;      :valid false,
;;      :index 9,
;;      :count 1,
;;      :p1 2,
;;      :p2 12}
;;     {:ch "n",
;;      :password "pckprbrnx",
;;      :valid false,
;;      :index 8,
;;      :count 1,
;;      :p1 5,
;;      :p2 7}
;;     {:ch "s",
;;      :password "hznnsqdx",
;;      :valid false,
;;      :index 5,
;;      :count 1,
;;      :p1 3,
;;      :p2 4}
;;     {:ch "k",
;;      :password "fwnchdkstg",
;;      :valid false,
;;      :index 7,
;;      :count 1,
;;      :p1 3,
;;      :p2 4}
;;     {:ch "h",
;;      :password "kpcnnpljchcqbcsvbq",
;;      :valid true,
;;      :index 10,
;;      :count 1,
;;      :p1 10,
;;      :p2 17}
;;     {:ch "c", :password "xvfcfkfz", :valid true, :index 4, :count 1, :p1 4, :p2 5})
