(ns day1
  (:require [clojure.string :as string]))

(defn find-result1 [numbers]
  (loop [lower-index 0
         upper-index (dec (count numbers))]
    (let [lower (nth numbers lower-index)
          upper (nth numbers upper-index)
          result (+ lower upper)]
      (cond
        (= result 2020) (* lower upper)
        (= lower-index upper-index) (throw (RuntimeException. "index match..."))
        (> result 2020) (recur lower-index (dec upper-index))
        (< result 2020) (recur (inc lower-index) upper-index)))))

(defn find-result2 [numbers]
  (let [lookup (set numbers)
        result (first (filter #(contains? lookup (- 2020 (+ (first %) (second %))))
                              (for [x1 numbers x2 numbers] [x1 x2])))]
    (if (not (nil? result))
      (let [[f s] result]
        (* f s (- 2020 (+ f s)))))))

(defn -main [& args]
  (let [input (slurp "inputs/day1")
        numbers (sort (map #(Integer/parseInt %) (string/split-lines input)))
        result1 (find-result1 numbers)
        result2 (find-result2 numbers)]
    (println result1)
    (println result2)))
