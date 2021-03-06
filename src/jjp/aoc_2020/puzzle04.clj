(ns jjp.aoc-2020.puzzle04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

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

(def real-input
  (slurp (io/reader (io/resource "puzzle_input04.txt")))
  )

(defn input->map [input]
  (mapv (fn [row]
          (walk/keywordize-keys (into {} (map #(str/split % #":") row)))
          )
        (mapv #(str/split % #"\s")
              (-> input
                  (str/split #"\n\n")
                  )))
  )

(defn valid-1? [rec]
  (let [req #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
        req-fk (set (keys (select-keys rec req)))
        valid? (= req-fk req)
        ]
    valid?
    )
  )

(count (filter identity (map valid-1? (input->map real-input))))
;; => 190

;; part 2
(s/def ::byr #(<= 1920 (Long/parseLong %) 2002))
(s/def ::iyr #(<= 2010 (Long/parseLong %) 2020))
(s/def ::eyr #(<= 2020 (Long/parseLong %) 2030))
(s/def ::hgt (s/or :c #(<= 150 (Long/parseLong (or (last (re-matches #"(\d+)cm" %)) "0")) 193)
                   :i #(<= 59 (Long/parseLong (or (last (re-matches #"(\d+)in" %)) "0")) 76)))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}?" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}?" %))

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid ]
                          :opt-un [::cid]))
(def valid-input "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(def invalid-input "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")
(map #(s/valid? ::passport %) (input->map valid-input))

(map #(s/valid? ::passport %) (input->map invalid-input))

(count (filter identity (map #(s/valid? ::passport %) (input->map real-input))))
;; => 121

;; lambda-island approach
(defn parse-entry [entry]
  (into {} (map (comp vec next)) (re-seq #"(\w{3}):(\S+)" entry)))

(defn parse-long [l]
  (Long/parseLong l))

(defn valid? [m]
  (= (count (dissoc m "cid")) 7))

(->> (str/split demo-input #"\R\R")
     (map parse-entry)
     (filter valid?)
     count)

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of:
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.

(defn valid2? [{:strs [byr iyr eyr hgt hcl ecl pid cid]}]
  (and
   byr (<= 1920 (parse-long byr) 2002)
   iyr (<= 2010 (parse-long iyr) 2020)
   eyr (<= 2020 (parse-long eyr) 2030)
   hgt (let [[_ num unit] (re-find #"(\d+)(in|cm)" hgt)]
         (case unit
           "cm" (<= 150 (parse-long num) 193)
           "in" (<= 59 (parse-long num) 76)
           false))
   hcl (re-find #"^#[0-9a-f]{6}$" hcl)
   ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
   pid (re-find #"^\d{9}$" pid)))

(->> (str/split real-input #"\R\R")
     (map parse-entry)
     (filter valid2?)
     count)
