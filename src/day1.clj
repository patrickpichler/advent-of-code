(ns day1
  (:require
    [clojure.string :as string]))

(defn load-inputs []
  (slurp "inputs/day1/input1"))

(defn part-1 [w]
  (- (Math/floor (/ w 3)) 2))

(defn part-2 [w]
  (->> 
    (iterate part-1 w)
    (drop 1)
    (take-while pos?)
    (reduce + 0)))

(defn -main [& args]
  (let [input (load-inputs)]
    (->> 
      (string/split-lines input)
      (map #(Integer/parseInt %1))
      (map part-1)
      (reduce + 0)
      println)
    (->> 
      (string/split-lines input)
      (map #(Integer/parseInt %1))
      (map part-2)
      (reduce + 0)
      println)))
