(ns day21
  (:require
   [util.intcode :as intcode]
   [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day21/input1"))

(def program-1
  "NOT A J
NOT C T
OR T J
AND D J
WALK

")


(defn part-1 []
  (let [input (load-input)
        args (doall (map int program-1))
        memory (intcode/parse-intcode-memory input)
        intcode-vm (intcode/create-intcode-vm memory args)
        [_ _ output] (intcode/execute-vm intcode-vm)]
    (println (string/join (map #(if (> % 255) % (char %)) (reverse output))))))

(def program-2
  "NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT E T
NOT T T
OR H T
AND T J
RUN
")

(defn part-2 []
  (let [input (load-input)
        args (doall (map int program-2))
        memory (intcode/parse-intcode-memory input)
        intcode-vm (intcode/create-intcode-vm memory args)
        [_ _ output] (intcode/execute-vm intcode-vm)]
    (println (string/join (map #(if (> % 255) % (char %)) (reverse output))))))

(defn -main [& args]
  (part-1)
  (part-2)
  )

