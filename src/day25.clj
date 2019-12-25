(ns day25
  (:require
   [util.intcode :as intcode]
   [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day25/input1"))

(defn print-text [input]
  (println (string/join (map char input))))

(defn run-ai
  ([] '())
  ([state input _]
   (print-text input)
   (let [action (read-line)]
     (intcode/create-computer-run-state :WAITING_FOR_INPUT state (reverse (concat (map int action) '(10)))))))

(defn part-1 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        vms (intcode/run {:1 (intcode/create-intcode-vm memory)
                          :ai (intcode/create-vm-state #'run-ai)}

                         {:1 [:ai]
                          :ai [:1]})
        [_ _ state] (:ai vms)]
    (map second (vals vms))))

(defn -main [& args]
  (println (part-1)))

