(ns day19
  (:require
   [util.intcode :as intcode]
   [clojure.string :as string]
   [clojure.algo.generic.math-functions :refer [sqrt pow]]))

(defn load-input []
  (slurp "inputs/day19/input1"))

(defn create-robot-state
  ([state stage result found-edges [x y :as new-pos]]
   (intcode/create-computer-run-state state [stage new-pos result found-edges] (list x y))))

(defn run-program-with-args [memory args]
  (nth (intcode/execute-vm (intcode/create-intcode-vm memory args)) 2))

(defn count-affected-area [memory start-pos movement]
  (loop [pos start-pos
         counter 0]
    (let [[state] (run-program-with-args memory pos)]
      (if (= pos [0 50]) counter
          (recur (movement pos)
                 (+ counter state))))))

(defn part-1 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        start-pos [0 0]]
    (count-affected-area memory start-pos (fn [[x y]] (case x
                                                        49 [0 (inc y)]
                                                        [(inc x) y])))))

(defn find-fitting-tractor-beam-size [memory [start-x start-y] size]
  (let [is-pos-affected (fn [pos] (= (first (run-program-with-args memory pos)) 1))]
    (loop [last-x start-x
           last-y start-y]
      (let [x (inc last-x)
            y (->> (iterate inc last-y)
                   (map #(vector % (is-pos-affected [x %])))
                   (drop-while (comp not second))
                   first
                   first)]
        (if (and (> (- x size) 0)
                 (> (- y size) 0))
          (let [big-enough (is-pos-affected [(- x size) (+ y size)])]
            (if big-enough [x y]
                (recur x y)))
          (recur x y))))))

(defn render [affected-positions]
  (let [affected (into #{} affected-positions)]
    (->> (for [y (range 0 49)
               x (range 0 49)]
           [x y])
         (map #(if (contains? affected %) \# \.))
         (partition-all 49)
         (map string/join)
         (string/join "\n")
         (println))))

(defn part-2 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        start-pos [20 0]
        size 99
        [x y] (find-fitting-tractor-beam-size memory start-pos size)]
    (+ (* 10000 (- x size)) y)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
