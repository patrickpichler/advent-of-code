(ns day4
  (:require [clojure.string :as string]))

(defn parse-passport [lines]
  (into {} (comp (mapcat #(string/split % #" "))
                 (map #(string/split % #":"))
                 (map (fn [[k v]] [(keyword k) v])))
        lines))

(comment
  (parse-passport ["hello:world wat:up" "howdy:hansi"]))

(into [] (comp (map inc))
      [1 2 3])

(defn has-all-keys? [passport]
  (every? (partial contains? passport)
          [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(defn solve-part-1 [input]
  (count (transduce (comp (partition-by #(= "" %))
                          (filter #(not= (first %) ""))
                          (map parse-passport)
                          (filter has-all-keys?))
                    conj (string/split-lines input))))

(comment
  (def i (slurp "inputs/day4"))
  (def lines (string/split-lines i))
  (solve-part-1 i)

  (transduce (comp (partition-by #(= "" %))
                   (filter #(not= (first %) "")))
             conj lines)

  (filter #(not= (first %) "") (partition-by #(= "" %) (string/split-lines i))))

(defn is-valid-birth-year? [birth-year]
  (and (re-matches #"\d{4,4}" birth-year)
       (<= 1920 (Integer/parseInt birth-year) 2002)))

(defn is-valid-issue-year? [issue-year]
  (and (re-matches #"\d{4,4}" issue-year)
       (<= 2010 (Integer/parseInt issue-year) 2020)))

(defn is-valid-expiration-year? [expiration-year]
  (and (re-matches #"\d{4,4}" expiration-year)
       (<= 2020 (Integer/parseInt expiration-year) 2030)))

(defn is-valid-height? [height]
  (if-let [[_ n u] (re-matches #"(\d+)(cm|in)" height)]
    (condp = u
      "cm" (<= 150 (Integer/parseInt n) 193)
      "in" (<= 59 (Integer/parseInt n) 76)
      false)
    false))

(defn is-valid-hair-color? [hcl]
  (some? (re-matches #"#[a-f0-9]{6,6}" hcl)))

(comment
  (is-valid-hair-color? "#aaaaaa")
  (is-valid-hair-color? "#aaaaay")
  (is-valid-hair-color? "#aaaaa1")
  (is-valid-hair-color? "#aaaaa21"))

(defn is-valid-eye-color? [ecl]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(comment
  (is-valid-eye-color? "amb"))

(defn is-valid-passport-id? [pid]
  (some? (re-matches #"[0-9]{9}" pid)))

(comment
  (is-valid-passport-id? "000000000")
  (is-valid-passport-id? "100000000")
  (is-valid-passport-id? "0000000000")
  (is-valid-passport-id? "808797855"))

(defn all-values-valid? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
   (is-valid-birth-year? byr)
   (is-valid-issue-year? iyr)
   (is-valid-expiration-year? eyr)
   (is-valid-height? hgt)
   (is-valid-hair-color? hcl)
   (is-valid-eye-color? ecl)
   (is-valid-passport-id? pid)))

(defn solve-part-2 [input]
  (count (transduce (comp (partition-by #(= "" %))
                          (filter #(not= (first %) ""))
                          (map parse-passport)
                          (filter has-all-keys?)
                          (filter all-values-valid?))
                    conj (string/split-lines input))))

(comment
  (solve-part-2 i))

(defn -main [& args]
  (let [input (slurp "inputs/day4")]
    (println (solve-part-1 input))
    (println (solve-part-2 input))))
