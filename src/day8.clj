(ns day8
  (:require [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day8/input1"))

(defn count-zeroes-in-layer [layer]
  (->> layer
       (filter (partial = 0))
       count))

(defn part-1 []
  (let [width 25
        height 6
        input (load-input)]
    (->> input
         (filter #(Character/isDigit %))
         (map #(Character/digit % 10))
         (partition-all (* width height))
         (sort-by count-zeroes-in-layer)
         first
         frequencies
         (#(* (or (% 1) 0)
              (or (% 2) 0))))))

(defn merge-pixel [& layers]
  (->> layers
       (drop-while #(= % 2))
       first))

(comment
  (merge-pixel [0 1 2 0]))

(defn digit->pixel [d]
  (case d
    0 "▓"
    1 "░"))

(defn print-line [line]
  (println (string/join (map digit->pixel line))))

(defn part-2 []
  (let [width 25
        height 6
        input (load-input)]
    (->> input
         (filter #(Character/isDigit %))
         (map #(Character/digit % 10))
         (partition-all (* width height))
         (apply map merge-pixel)
         (partition-all width)
         (run! print-line))))

(defn -main [& args]
  (println (part-1))
  (part-2))
