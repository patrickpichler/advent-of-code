(ns day16
  (:require [clojure.string :as string]
            [clojure.algo.generic.math-functions :refer [abs]]))

(defn load-input []
  (slurp "inputs/day16/input1"))

(defn parse-input [input]
  (->> input
       (filter #(Character/isDigit ^char %))
       (map #(Long/parseLong (Character/toString %)))
       (into [])))

(def base-pattern '(0 1 0 -1))

(defn generate-pattern [n]
  (let [pos (inc n)]
    (->> base-pattern
         cycle
         (drop 1)
         (mapcat #(repeat pos %)))))

(defn get-ones-digit [n]
  (Math/abs ^long (rem n 10)))

(defn calculate-line [input]
  (let [input-length (count input)
        half (int (/ input-length 2))]
    (->> (range 0 (count input))
         (map (fn [n]
                (if (<=  n half)
                  (get-ones-digit (reduce + (map * (drop n input) (generate-pattern n))))
                  (get-ones-digit (reduce + (map #(nth input %) (range n input-length)))))))
         (into []))))

(defn part-1 []
  (let [input (load-input)
        parsed-input (parse-input input)]

    (->> (iterate calculate-line parsed-input)
         (take 101)
         last
         (take 8)
         string/join)))

(defn calculate-line-fast [input]
  (let [reversed-input (rseq input)]
    (transduce identity (fn
                          ([] [0 '()])
                          ([[_ result]] (vec result))
                          ([[current-sum result] elem]
                           (let [sum (+ current-sum elem)]
                             [sum (cons (get-ones-digit sum) result)])))
               reversed-input)))

(defn part-2 []
  (let [input (load-input)
        to-skip (Integer/parseInt (clojure.string/join (take 7 input)))
        parsed-input (parse-input input)
        puzzle-input (into [] (mapcat identity (repeat 10000 parsed-input)))
        trimed-puzzle-input (vec (drop to-skip puzzle-input))]

    (->> (iterate calculate-line-fast trimed-puzzle-input)
         (drop 100)
         first
         (take 8)
         string/join)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
