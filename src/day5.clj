(ns day5
  (:require
   [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day5/input1"))

(defn load-and-parse-input []
  (as->
   (load-input) x
    (string/split x #"[,\n]")
    (map #(Integer/parseInt %1) x)
    (into [] x)))

(defn get-digits [number]
  (for [n (->> number
               (iterate #(int (/ % 10)))
               (take-while #(> % 0)))]
    (rem n 10)))

(defn parse-instruction [instruction]
  (let [digits (reverse (get-digits instruction))
        instr (mod instruction 100)]
    (case (count digits)
      1 [0 0 0 instr]
      2 [0 0 0 instr]
      3 [(nth digits 0) 0 0 instr]
      4 [(nth digits 1) (nth digits 0) 0 instr]
      5 [(nth digits 2) (nth digits 1) (nth digits 0) instr])))

(defn get-input [memory mode value]
  (case mode
    0 (nth memory value)
    1 value))

(defn run-computer [memory input pos]
  (if (< pos (count memory))
    (let [[p1 p2 p3 instr] (parse-instruction (nth memory pos))]
      (case instr
        1 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
            (recur (assoc memory dest (+ v1 v2)) input (+ pos 4)))
        2 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
            (recur (assoc memory dest (* v1 v2)) input (+ pos 4)))
        3 (let [[dest] (subvec memory (inc pos) (+ pos 2))
                value (first input)]
            ; (println instr dest " -- " value " -- " p1 p2 p3)
            (println "Input:\t" value)
            (recur (assoc memory dest value) (rest input) (+ pos 2)))
        4 (let [[source] (subvec memory (inc pos) (+ pos 2))
                value (get-input memory p1 source)]
            ; (println instr source " -- " value " -- " p1 p2 p3)
            (println "Output:\t" value)
            (recur memory input (+ pos 2)))
            ; memory)
        99 memory))
    memory))

(defn part-1 []
  (let [memory (load-and-parse-input)]
    (run-computer memory [1] 0)))

(defn run-computer-2 [memory input pos]
  (if (< pos (count memory))
    (let [raw-instr (nth memory pos)
          [p1 p2 p3 instr] (parse-instruction raw-instr)]
      (case instr
        1 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
            (recur (assoc memory dest (+ v1 v2)) input (+ pos 4)))

        2 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
            (recur (assoc memory dest (* v1 v2)) input (+ pos 4)))

        3 (let [[dest] (subvec memory (inc pos) (+ pos 2))
                value (first input)]
            ; (println raw-instr dest " -- " value " -- " p1 p2 p3)
            (println "Input:\t" value)
            (recur (assoc memory dest value) (rest input) (+ pos 2)))

        4 (let [[source] (subvec memory (inc pos) (+ pos 2))
                value (get-input memory p1 source)]
            ; (println raw-instr source " -- " value " -- " p1 p2 p3)
            (println "Output:\t" value)
            (recur memory input (+ pos 2)))

        5 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2)
            (if (not= 0 v1)
              (recur memory input v2)
              (recur memory input (+ pos 3))))

        6 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)]
            ; (println raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2)
            (if (= 0 v1)
              (recur memory input v2)
              (recur memory input (+ pos 3))))

        7 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)
                value (if (< v1 v2) 1 0)]
            ; (println instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value)
            (recur (assoc memory dest value) input (+ pos 4)))

        8 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                v1 (get-input memory p1 s1)
                v2 (get-input memory p2 s2)
                value (if (= v1 v2) 1 0)]
            ; (println raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value)
            (recur (assoc memory dest value) input (+ pos 4)))

        99 memory))
    memory))

(defn part-2 []
  (let [memory (load-and-parse-input)]
    (run-computer-2 memory [5] 0)))

(defn -main [& args]
  (println "------------- Part 1 -----------------")
  (part-1)
  (println)
  (println "------------- Part 2 -----------------")
  (part-2))
