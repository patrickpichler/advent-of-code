(ns day10
  (:require [util :refer [load-lines]]))

(defn solve-part-1 [input]
  (let [sorted (sort input)]
    (->> (concat '(0) sorted (list (+ 3 (last sorted))))
         (partition 2 1)
         (map #(- (second %) (first %)))
         frequencies)))

(defn -main [& args]
  (let [input (map #(Long/parseLong %) (load-lines 10))
        {one 1 three 3} (solve-part-1 input)]
    (println (* one three))))
