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

(defn distance [[ax ay] [bx by]]
  (Math/sqrt (+ (Math/pow (- ax bx) 2) (Math/pow (- ay by) 2))))

(defn equal-enought? [x1 x2]
  (< (Math/abs (- x1 x2)) 0.00001))

(defn is-point-on-line? [[a b] c]
  (let [d1 (distance a c)
        d2 (distance b c)
        d3 (distance a b)]
    (equal-enought? (+ d1 d2) d3)))

(comment
  (is-point-on-line? [[0 0] [0 4]] [0 2])
  (is-point-on-line? [[0 0] [4 4]] [2 2])
  (is-point-on-line? [[0 0] [1 1]] [2 2])
  (is-point-on-line? [[4 4] [1 1]] [2 2]))

(defn is-direct-sight? [astroids [a1 a2 :as line]]
  (->> astroids
       (filter #(and (not= % a1) (not= % a2)))
       (some (partial is-point-on-line? line))
       not))

(defn increase [x]
  (if (nil? x) 1
      (inc x)))

(defn increase-astroid-visible-count [m [a1 a2]]
  (-> m
      (update a1 increase)
      (update a2 increase)))

(defn part-1 []
  (let [astroids (parse-input)]
    (->> (combo/combinations astroids 2)
         (filter (partial is-direct-sight? astroids))
         (reduce increase-astroid-visible-count {})
         (reduce (fn [[p c :as a] [p2 c2 :as a2]]
                   (if (> c c2) a a2))))))

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
  (get-angle [11 13] [11 12])
  )

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
  (let [
        base-astroid [23 19]
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
  (println (part-1)
  (println (part-2)))
