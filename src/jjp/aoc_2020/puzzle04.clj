(ns jjp.aoc-2020.puzzle04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def labels {:byr "Birth Year"
             :iyr "Issue Year"
             :eyr "Expiration Year"
             :hgt "Height"
             :hcl "Hair Color"
             :ecl "Eye Color"
             :pid "Passport ID"
             :cid "Country ID"})

(def demo-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input
  (slurp (io/reader (io/resource "puzzle_input04.txt")))
  )

(defn input->map [input]
  (mapv (fn [row]
          (into {} (map #(str/split % #":") row))
          )
        (mapv #(str/split % #"\s")
              (-> input
                  (str/split #"\n\n")
                  )))
  )

(defn valid? [rec]
  (let [req #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}
        req-fk (set (keys (select-keys rec req)))
        valid? (= req-fk req)
        ]
    valid?
    )
  )

(count (filter identity (map valid? (input->map input))))
;; => 190
