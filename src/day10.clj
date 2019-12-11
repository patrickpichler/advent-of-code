(ns day10
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn load-input []
  (slurp "inputs/day10/input1"))

(defn parse-line [row line]
  (->> line
       (keep-indexed #(if (= %2 \#) [%1 row] nil))
       (into [])))

(defn parse-input []
  (->> (load-input)
       string/split-lines
       (map-indexed #(parse-line %1 %2))
       (apply concat)))

(defn to-polar [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        length (Math/sqrt (+ (* dx dx) (* dy dy)))
        angle (Math/atan2 dy dx)
        angleInDegrees (mod (+ (Math/toDegrees angle) 360) 360)]
    [angleInDegrees length]))

(defn get-number-of-distinct-polars-per-astroid [astroids]
  (->> astroids
       (map (fn [a]
              (->> astroids
                   (filter #(not= a %))
                   (map (partial to-polar a))
                   (map first)
                   distinct
                   count
                   ((partial vector a)))))
       (into {})))

(defn part-1 []
  (let [astroids (parse-input)
        polars-per-astroid (get-number-of-distinct-polars-per-astroid astroids)]
    (->> polars-per-astroid
         (reduce #(if (> (second %1) (second %2)) %1 %2)))))

(def number-of-decimal-places 100000)

(defn round-angle [a]
  (Math/round (* a number-of-decimal-places)))

(defn get-angle [[x1 y1] [x2 y2]]
  (round-angle (let [result (Math/toDegrees (Math/atan2 (- y2 y1)  (- x2 x1)))]
                 (if (< result 0) (+ 360 result)
                     result))))

(comment
  (get-angle [2 2] [2 1])
  (get-angle [2 2] [3 2])
  (get-angle [2 2] [2 3])
  (get-angle [2 2] [1 2])
  (get-angle [11 13] [11 12]))

(defn distance [[ax ay] [bx by]]
  (Math/sqrt (+ (Math/pow (- ax bx) 2) (Math/pow (- ay by) 2))))

(defn collect-astroids-on-angles [base-astroid astroids]
  (->> astroids
       (filter #(not= base-astroid %))
       (map #(vector (get-angle base-astroid %) [% (distance base-astroid %)]))
       (group-by first)
       (map (fn [[k v]]
              (let [sorted-list (sort-by second (map second v))]
                (vector k sorted-list))))
       (into {})))

(defn part-2 []
  (let [base-astroid [23 19]
        astroids (parse-input)
        astroids-by-angle (collect-astroids-on-angles base-astroid astroids)
        start-angle (* 270 number-of-decimal-places)
        astroids-by-angle-size (count astroids-by-angle)]
    (->> astroids-by-angle
         keys
         sort
         cycle
         (drop-while #(< % start-angle))
         (map astroids-by-angle)
         (keep-indexed #(let [run (int (/ %1 astroids-by-angle-size))]
                          (nth %2 run nil)))
         (take 200)
         last
         ((fn [[[x y] _]] (+ (* 100 x) y))))))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
