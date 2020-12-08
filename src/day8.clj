(ns day8
  (:require [clojure.string :as string]))

(defn parse-instruction [i]
  (let [[cmd arg] (string/split i #" " 2)]
    [(keyword cmd) (Integer/parseInt arg)]))

(defn run-instructions [instructions]
  (loop [idx 0
         acc 0
         visited #{}]
    (if (not (contains? visited idx))
      (if-let [[instr arg] (nth instructions idx nil)]
        (condp = instr
          :acc (recur (inc idx) (+ acc arg) (conj visited idx))
          :nop (recur (inc idx) acc (conj visited idx))
          :jmp (recur (+ idx arg) acc (conj visited idx)))
        [true acc])
      [false acc])))

(defn swap-instruction [instructions idx]
  (let [[i arg] (nth instructions idx)
        replacement (condp = i
                      :acc (throw (RuntimeException. "Cannot swap acc"))
                      :nop [:jmp arg]
                      :jmp [:nop arg])]
    (assoc instructions idx replacement)))

(defn run-instructions-auto-heal [instructions]
  (let [swapable-idxs (map first
                           (filter #(#{:jmp :nop} (first (second %)))
                                   (map-indexed vector instructions)))]
    (->> swapable-idxs
         (map #(swap-instruction instructions %))
         (map run-instructions)
         (filter first)
         first)))

(defn -main [& args]
  (let [lines (string/split-lines (slurp "inputs/day8"))
        instructions (mapv parse-instruction lines)]
    (println (second (run-instructions instructions)))
    (println (second (run-instructions-auto-heal instructions)))))
