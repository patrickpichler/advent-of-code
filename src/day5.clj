(ns day5
  (:require [clojure.string :as string]))

(defn find-seat [code]
  (let [binary (string/replace (string/replace code #"[FL]" "0") #"[RB]" "1")]
    (Integer/parseInt binary 2)))

(comment
  (def i (slurp "inputs/day5"))
  (def lines (string/split-lines i))

  (find-seat "FBFBBFFRLR")

  (Integer/parseInt "010" 2)

  (reduce max (map find-seat lines))

  (split-at 8 "FBFBBFFRLR"))

(defn find-my-seat [codes]
  (let [seats (sort (map find-seat codes))]
    (->> (map vector seats (rest seats))
         (filter (fn [[a b]] (= (- b a) 2)))
         (map #(inc (first %)))
         first)))

(comment
  (map #(vector %1 %2) (range 10) (range 1 10))

  (find-my-seat lines)
  (time (find-my-seat lines))
  )

(defn -main [& args]
  (let [input (slurp "inputs/day5")
        lines (string/split-lines input)]
    (println (reduce max (map find-seat lines)))
    (println (find-my-seat lines))))
