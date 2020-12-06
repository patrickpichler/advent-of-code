(ns day6
  (:require [clojure.string :as string]
            [clojure.set :as s]))

(comment
  (def sample "abc

a
b
c

ab
ac

a
a
a
a

b"))

(defn count-all-answers-part1 [lines]
  (transduce (comp (partition-by #(= % ""))
                   (filter #(not= (first %) ""))
                   (map #(count (into #{} (apply str %)))))
             + lines))

(comment
  (def l (string/split-lines (slurp "inputs/day6")))
  (count-all-answers-part1 (string/split-lines sample))
  (count-all-answers-part1 l))

(defn find-intersections [answers]
  (apply s/intersection (map set answers)))

(defn count-all-answers-part2 [lines]
  (transduce (comp (partition-by #(= % ""))
                   (filter #(not= (first %) ""))
                   (map find-intersections)
                   (map count))
             + lines))

(comment
  (def l (string/split-lines (slurp "inputs/day6")))

  (count-all-answers-part2 (string/split-lines sample))
  (count-all-answers-part2 l))

(defn -main [& args]
  (let [input (slurp "inputs/day6")
        lines (string/split-lines input)]
    (println (count-all-answers-part1 lines))
    (println (count-all-answers-part2 lines))))
