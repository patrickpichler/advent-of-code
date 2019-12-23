(ns day23
  (:require
   [util.intcode :as intcode]))

(defn load-input []
  (slurp "inputs/day23/input1"))

(defn parse-output [output]
  (loop [result {}
         to-process (reverse output)]
    (if (empty? to-process) result
        (let [[dest x y & r] to-process]
          (recur (update result dest concat (list x y))
                 r)))))

(defn store-input
  ([] nil)
  ([state input _] (intcode/create-computer-run-state (if state :KILL_COMPUTER :WAITING_FOR_INPUT) input)))

(defn part-1 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        intcode-vms (->> (range 0 50)
                         (map #(vector % (intcode/create-intcode-vm memory [%])))
                         (into {}))

        vms (intcode/run-extended (assoc intcode-vms 255 (intcode/create-vm-state #'store-input))
                                  (fn [_ output]
                                    (loop [result {}
                                           to-process (reverse output)]
                                      (if (empty? to-process) result
                                          (let [[dest x y & r] to-process]
                                            (recur (update result dest concat [x y])
                                                   r)))))
                                  (fn [_ input]
                                    (concat input [-1])))
        [_ _ [_ y]] (vms 255)]
    y))

(defn nat
  ([] [:NEW])
  ([[state [last-x last-y :as l] [last-send-x last-send-y :as last-send]] input _]
   (let [[x y] (take-last 2 (concat l input))]
     (cond
       (= state :NEW) (intcode/create-computer-run-state :WAITING_FOR_INPUT [:AWAIT])

       (= state :AWAIT) (do
                          (if (= last-send-y y)
                            (intcode/create-computer-run-state :KILL_COMPUTER y)
                            (intcode/create-computer-run-state :OUTPUT_READY
                                                               [:READY [x y] [x y]]
                                                               [y x 0])))

       (= state :READY) (intcode/create-computer-run-state :OUTPUT_READY [:AWAIT [x y] last-send])))))

(defn part-2 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        intcode-vms (->> (range 0 50)
                         (map #(vector % (intcode/create-intcode-vm memory [%])))
                         (into {}))

        vms (intcode/run-extended (assoc intcode-vms 255 (intcode/create-vm-state #'nat))
                                  (fn [id output]
                                    (loop [result {}
                                           to-process (reverse output)]
                                      (if (empty? to-process) result
                                          (let [[dest x y & r] to-process]
                                            (recur (update result dest concat [x y])
                                                   r)))))
                                  (fn [id input]
                                    (if (= id 255) input
                                        (concat input [-1]))))
        [_ _ y] (vms 255)]
    y))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
