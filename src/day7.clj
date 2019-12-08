(ns day7
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]))

(def debug-active false)

(defmacro debug [& args]
  `(if ~debug-active
     (println "DEBUG:" ~@args)))

(defn load-input []
  (slurp "inputs/day7/input1"))

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

(defn run-computer
  ([memory input pos] (run-computer memory input pos []))
  ([memory input pos output]
   (if (< pos (count memory))
     (let [raw-instr (nth memory pos)
           [p1 p2 p3 instr] (parse-instruction raw-instr)]
       (case instr
         1 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
             (recur (assoc memory dest (+ v1 v2))
                    input
                    (+ pos 4)
                    output))

         2 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2)
             (recur (assoc memory dest (* v1 v2))
                    input
                    (+ pos 4)
                    output))

         3 (let [[dest] (subvec memory (inc pos) (+ pos 2))
                 value (first input)]
             (debug raw-instr dest " -- " value " -- " p1 p2 p3)
             (debug "Input:\t" value " === " input)
             (recur (assoc memory dest value)
                    (rest input)
                    (+ pos 2)
                    output))

         4 (let [[source] (subvec memory (inc pos) (+ pos 2))
                 value (get-input memory p1 source)]
             (debug raw-instr source " -- " value " -- " p1 p2 p3)
             (debug "Output:\t" value)
             (recur memory
                    input
                    (+ pos 2)
                    (cons value output)))

         5 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2)
             (if (not= 0 v1)
               (recur memory input v2 output)
               (recur memory input (+ pos 3) output)))

         6 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2)
             (if (= 0 v1)
               (recur memory input v2 output)
               (recur memory input (+ pos 3) output)))

         7 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)
                 value (if (< v1 v2) 1 0)]
             (debug instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value)
             (recur (assoc memory dest value)
                    input
                    (+ pos 4)
                    output))

         8 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)
                 value (if (= v1 v2) 1 0)]
             (debug raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value)
             (recur (assoc memory dest value)
                    input
                    (+ pos 4)
                    output))

         99 [memory output]))
     memory)))

(defn run-with-args [memory args]
  (loop [input-sequence args
         last-run 0]
    (let [next-input (first input-sequence)]
      (if (nil? next-input) last-run
          (let [input (vector next-input last-run)
                [_ result] (run-computer memory input 0)]
            (recur (rest input-sequence) (first result)))))))

(defn part-1 []
  (let [memory (load-and-parse-input)]
    (->> (combo/permutations [0 1 2 3 4])
         (map (partial run-with-args memory))
         (reduce max))))

(defn create-state
  ([state memory output pos] (create-state state memory output pos []))
  ([state memory output pos args]
   [state memory output pos (or args [])]))

(defn get-output [[_ _ output _ _ :as vm]]
  
  (if (= (count output) 1)
    (first output)
    (do
      (println "WARNING: More than one or no output: " output)
      -1)))

(defn run-computer-1
  ([memory input pos output args] (run-computer-1 memory
                                                  (concat args input)
                                                  pos
                                                  output))
  ([memory input pos output]
   (if (< pos (count memory))
     (let [raw-instr (nth memory pos)
           [p1 p2 p3 instr] (parse-instruction raw-instr)]
       (case instr
         1 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug "ADD" raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2 " :: " pos)
             (recur (assoc memory dest (+ v1 v2))
                    input
                    (+ pos 4)
                    output))

         2 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug "MUL" raw-instr s1 s2 dest " -- " p1 p2 p3 " -- " v1 v2 " :: " pos)
             (recur (assoc memory dest (* v1 v2))
                    input
                    (+ pos 4)
                    output))

         3 (let [[dest] (subvec memory (inc pos) (+ pos 2))
                 value (first input)]

             (debug "INPUT" raw-instr dest " -- " value " -- " p1 p2 p3 " :: " pos)
             (debug "Input:\t" value " === " input)

             (if (nil? value)
               (do
                 (debug "Input nil:\t Suspend and wait for IO")
                 (create-state :WAITING memory output pos))

               (recur (assoc memory dest value)
                      (rest input)
                      (+ pos 2)
                      output)))

         4 (let [[source] (subvec memory (inc pos) (+ pos 2))
                 value (get-input memory p1 source)]
             (debug "OUTPUT" raw-instr source " -- " value " -- " p1 p2 p3 " :: " pos)
             (debug "Output:\t" value)
             (recur memory
                    input
                    (+ pos 2)
                    (cons value output)))

         5 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug "JUMP-NOT" raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " :: " pos)
             (if (not= 0 v1)
               (recur memory input v2 output)
               (recur memory input (+ pos 3) output)))

         6 (let [[s1 s2] (subvec memory (inc pos) (+ pos 3))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)]
             (debug "JUMP" raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " :: " pos)
             (if (= 0 v1)
               (recur memory input v2 output)
               (recur memory input (+ pos 3) output)))

         7 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)
                 value (if (< v1 v2) 1 0)]
             (debug "LESS" instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value " :: " pos)
             (recur (assoc memory dest value)
                    input
                    (+ pos 4)
                    output))

         8 (let [[s1 s2 dest] (subvec memory (inc pos) (+ pos 4))
                 v1 (get-input memory p1 s1)
                 v2 (get-input memory p2 s2)
                 value (if (= v1 v2) 1 0)]
             (debug "EQ" raw-instr s1 s2 " -- " p1 p2 p3 " -- " v1 v2 " -- " value " :: " pos)
             (recur (assoc memory dest value)
                    input
                    (+ pos 4)
                    output))

         99 (do
              (debug "VM terminated :: " pos)
              (create-state :TERMIATED memory output pos))))
     memory)))

(defn create-vm [memory args]
  (create-state :NEW memory [] 0 args))

(defn has-parent-output [vms pipes reversed-pipes vm-id]
  (let [[state _ _ output] (vms vm-id)]
    (empty? output)))

(defn is-vm-runnable [vms pipes reversed-pipes [id [state]]]
  (case state
    :NEW true
    :TERMIATED false
    :WAITING (let [input-vm-id (reversed-pipes id)
                   [_ _ input-vm-output] (vms input-vm-id)]
               (not (empty? input-vm-output)))))

(defn consume-output [[state memory _ pos args]]
  (create-state state memory [] pos args))

(defn run-vm [vms pipes reversed-pipes [id [_ memory output pos args]]]
  (let [input-vm-id (reversed-pipes id)
        [_ _ input-output _ _ :as input-vm] (vms input-vm-id)
        _ (debug "Run vm " id " with input " input-output " and args " args)
        [s _ o p :as result] (run-computer-1 memory input-output pos output args)
        _ (debug "VM " id " suspended with state " s " output " o " pos " p)
        updated-vms (assoc vms
                           input-vm-id (consume-output input-vm)
                           id result)]
    updated-vms))

(defn find-vm-to-run [vms pipes reversed-pipes]
  (->> vms
       (filter #(is-vm-runnable vms pipes reversed-pipes %))
       first))

(defn get-state [[id [state]]]
  state)

(defn vm-terminated? [vm]
  (= (get-state vm) :TERMIATED))

(defn retrieve-result [vms vm-id]
  (if (every? vm-terminated? vms)
    (let [result (get-output (vms vm-id))]
      (debug "Result\t" result)
      result)
    (do
      (println "ERROR: NOT ALL VMS TERMINATED!")
      (println "States: " (map get-state vms))
      -1)))

(defn run-with-args-2 [memory [a1 a2 a3 a4 a5]]
  (loop [vms {:0 (create-vm (with-meta memory {:vm :0}) [a1 0])
              :1 (create-vm (with-meta memory {:vm :1}) [a2])
              :2 (create-vm (with-meta memory {:vm :2}) [a3])
              :3 (create-vm (with-meta memory {:vm :3}) [a4])
              :4 (create-vm (with-meta memory {:vm :4}) [a5])}

         pipes {:0 :1
                :1 :2
                :2 :3
                :3 :4
                :4 :0}
         reverse-pipes (into {} (map (comp vec reverse) pipes))]
    (let [vm-to-run (find-vm-to-run vms pipes reverse-pipes)]
      (if (nil? vm-to-run) (retrieve-result vms :4)
          (let [updated-vms (run-vm vms pipes reverse-pipes vm-to-run)]
            (recur updated-vms pipes reverse-pipes))))))

(defn part-2 []
  (let [memory (load-and-parse-input)]
    (->> (combo/permutations [5 6 7 8 9])
         (map (partial run-with-args-2 memory))
         (reduce max))))

(defn -main [& args]
  (println "------------- Part 1 -----------------")
  (println (part-1))
  (println)
  (println "------------- Part 2 -----------------")
  (println (part-2)))
