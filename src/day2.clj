(ns day2
  (:require
   [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day2/input1"))

(defn load-and-parse-input []
  (as->
   (load-input) x
    (string/split x #"[,\n]")
    (map #(Integer/parseInt %1) x)
    (into [] x)))

(defn run-computer [memory pos]
  (if (< pos (count memory))
    (let [instruction (nth memory pos)]
      (case instruction
        1 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (nth memory s1)
                v2 (nth memory s2)]
            (recur (assoc memory dest (+ v1 v2)) (+ pos 4)))
        2 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (nth memory s1)
                v2 (nth memory s2)]
            (recur (assoc memory dest (* v1 v2)) (+ pos 4)))
        99 memory))
    memory))

(defn run-computer-with-args [memory arg1 arg2]
  (let [start-memory (assoc (assoc memory 1 arg1) 2 arg2)]
    (nth (run-computer start-memory 0) 0)))

(defn part2 [input]
  (->> (for [x (range 0 100)
             y (range 0 100)]
         [x y])
       (some (fn [[x y :as p]] (if (=
                                    (run-computer-with-args input x y)
                                    19690720)
                                 p)))))

(defn -main [& args]
  (let [input (load-and-parse-input)
        result-1 (run-computer-with-args input 12 2)
        [a b] (part2 input)]
    (println result-1)
    (println (+ (* 100 a) b))))
