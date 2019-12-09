(ns day9
  (:require [clojure.string :as string]))

(def debug-active false)

(defmacro debug [& args]
  `(if ~debug-active
     (println "DEBUG:" ~@args)))

(defn load-input []
  (slurp "inputs/day9/input1"))
  ; (slurp "sample"))

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

(defn create-state
  ([state memory output pos]
   (create-state state memory output pos []))

  ([state memory output pos args]
   [state memory output pos (or args [])]))

(defn get-output [[_ _ output _ _]]
  (vec (reverse output)))

(defn create-memory
  ([memory] (create-memory memory 0))
  ([memory relative-pos]
   [memory {} relative-pos]))

(defn get-from-memory [[memory extended] addr]
  (if (< addr (count memory)) (nth memory addr)
      (extended addr 0)))

(defn get-relative-from-memory [[_ _ relative-pos :as m] offset]
  (let [addr (+ relative-pos offset)]
    (get-from-memory m addr)))

(defn set-in-memory [[memory extended relative-pos] addr value]
  (if (< addr (count memory))
    [(assoc memory addr value) extended relative-pos]
    [memory (assoc extended addr value) relative-pos]))

(defn set-relative-in-memory [[_ _ relative-pos :as m] offset value]
  (let [addr (+ relative-pos offset)]
    (set-in-memory m addr value)))

(defn get-input [memory mode value]
  (case mode
    0 (get-from-memory memory value)
    1 value
    2 (get-relative-from-memory memory value)))

(defn set-output [memory mode addr value]
  (case mode
    0 (set-in-memory memory addr value)
    1 (set-in-memory memory addr value)
    2 (set-relative-in-memory memory addr value)))

(defn load-from-extended [[_ extended] from size]
  (->> (range from (+ from size))
       (map #(or (extended %) 0))
       (into [])))

(defn load-from-real [[real] from size]
  (subvec real from (+ from size)))

(defn load-chunk-from-memory [[real :as m] from size]
  (let [real-memory-size (count real)]
    (cond
      (< (+ from size) real-memory-size) (load-from-real m from size)
      (< from real-memory-size) (concat (load-from-real m
                                                        from
                                                        (- real-memory-size from))
                                        (load-from-extended m
                                                            real-memory-size
                                                            (- (+ from size) real-memory-size)))
      :else (load-from-extended m from size))))

(defn modify-relative-position-by [[_ _ relative-pos :as memory] offset]
  (assoc memory 2 (+ offset relative-pos)))

(defmacro step [& kvs]
  (let [args (apply assoc {} kvs)]
    (list 'recur
          (or (args :memory) 'memory)
          (or (args :input) 'input)
          (or (cond (contains? args :pos) (args :pos)
                    (contains? args :inc-pos) (list '+ 'pos (args :inc-pos)))
              'pos)
          (or (args :output) 'output)
          (list 'inc 'tick))))

(defmacro load-instructions [destrs memory pos expr]
  (list 'let [destrs (list 'load-chunk-from-memory memory (list 'inc pos) (count destrs))]
        expr))

(defn dump-memory [[real extended relative-pos] tick pos]
  (let [memory-string (string/join "\n" (concat (map-indexed vector real) (map identity extended)))]
    (spit (str "memory-dumps/" tick) (str "relative-pos: " relative-pos "\n"
                                          "pos: " pos "\n"
                                          memory-string))))

(defn run-computer
  ([memory input pos output] (run-computer memory input pos output 0))
  ([memory input pos output tick]
   ; (dump-memory memory tick pos)
   (let [raw-instr (get-from-memory memory pos)
         [p1 p2 p3 instr] (parse-instruction raw-instr)]
     (case instr
       1 (load-instructions [s1 s2 dest] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)
                                  result (+ v1 v2)]
                              (debug tick "\t" pos "\t" "ADD" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 s2 dest  "  " v1 "+" v2 " = " result)
                              (step :memory (set-output memory p3 dest result)
                                    :inc-pos 4)))

       2 (load-instructions [s1 s2 dest] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)
                                  result (* v1 v2)]
                              (debug tick "\t" pos "\t" "MUL" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 s2 dest  "  " v1 "*" v2 " = " result)
                              (step :memory (set-output memory p3 dest result)
                                    :inc-pos 4)))

       3 (load-instructions [dest] memory pos
                            (let [value (first input)]

                              (debug tick "\t" pos "\t" "IN" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr dest  " VALUE: " value)

                              (if (nil? value)
                                (do
                                  (debug "Input nil:\t Suspend and wait for IO")
                                  (create-state :WAITING memory output pos))

                                (step :memory (set-output memory p1 dest value)
                                      :input (rest input)
                                      :inc-pos 2))))

       4 (load-instructions [source] memory pos
                            (let [value (get-input memory p1 source)]
                              (debug tick "\t" pos "\t" "OUT" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr source  " PRINT " value)
                              (step :inc-pos 2
                                    :output (cons value output))))

       5 (load-instructions [s1 s2] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)]
                              (debug tick "\t" pos "\t" "JMP" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 s2  " " v1 "!= 0 ==> " v2)
                              (if (not= 0 v1)
                                (step :pos v2)
                                (step :inc-pos 3))))

       6 (load-instructions [s1 s2] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)]
                              (debug tick "\t" pos "\t" "JMP-N" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 s2  " " v1 "== 0 ==> " v2)
                              (if (= 0 v1)
                                (step :pos v2)
                                (step :inc-pos 3))))

       7 (load-instructions [s1 s2 dest] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)
                                  value (if (< v1 v2) 1 0)]
                              (debug tick "\t" pos "\t" "LESS" "\t" "MODES:" p1 p2 p3 " INSTRS:" instr s1 s2  " " v1 < v2 " == " value)
                              (step :memory (set-output memory p3 dest value)
                                    :inc-pos 4)))

       8 (load-instructions [s1 s2 dest] memory pos
                            (let [v1 (get-input memory p1 s1)
                                  v2 (get-input memory p2 s2)
                                  value (if (= v1 v2) 1 0)]
                              (debug tick "\t" pos "\t" "EQ" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 s2 dest " " v1 "==" v2 "=" value)
                              (step :memory (set-output memory p3 dest value)
                                    :inc-pos 4)))

       9 (load-instructions [s1] memory pos
                            (let [value (get-input memory p1 s1)]
                              (debug tick "\t" pos "\t" "REL" "\t" "MODES:" p1 p2 p3 " INSTRS:" raw-instr s1 " SET " value)
                              (step :memory (modify-relative-position-by memory value)
                                    :inc-pos 2)))

       99 (do
            (debug "VM terminated :: " pos)
            (create-state :TERMIATED memory output pos))))))

(defn run-computer-with-args [memory input pos output args]
  (run-computer memory (concat args input) pos output))

(defn create-vm
  ([memory]
   (create-vm memory []))

  ([memory args]
   (create-state :NEW (create-memory memory) [] 0 args)))

(defn is-vm-runnable [vms reversed-pipes [id [state]]]
  (case state
    :NEW true
    :TERMIATED false
    :WAITING (let [input-vm-id (reversed-pipes id)
                   [_ _ input-vm-output] (vms input-vm-id)]
               (not (empty? input-vm-output)))))

(defn consume-output [[state memory _ pos args]]
  (create-state state memory [] pos args))

(defn run-vm [vms reversed-pipes [id [_ memory output pos args]]]
  (let [input-vm-id (reversed-pipes id)
        [_ _ input-output _ _ :as input-vm] (vms input-vm-id)
        vm-input (or input-output [])
        _ (debug "Run vm " id " with input " vm-input " and args " args)
        [s _ o p :as result] (run-computer-with-args memory vm-input pos output args)
        _ (debug "VM " id " suspended with state " s " output " o " pos " p)
        updated-vms (if (nil? input-vm)
                      (assoc vms id result)
                      (assoc vms
                             input-vm-id (consume-output input-vm)
                             id result))]
    updated-vms))

(defn find-vm-to-run [vms reversed-pipes]
  (->> vms
       (filter #(is-vm-runnable vms reversed-pipes %))
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

(defn run
  ([vms] (run vms {}))
  ([vms-to-run pipes]
   (loop [vms vms-to-run
          reverse-pipes (into {} (map (comp vec reverse) pipes))]

     (let [vm-to-run (find-vm-to-run vms reverse-pipes)]
       (if (nil? vm-to-run) (retrieve-result vms :1)
           (let [updated-vms (run-vm vms reverse-pipes vm-to-run)]
             (recur updated-vms reverse-pipes)))))))

(defn part-1 []
  (let [memory (load-and-parse-input)]
    (run {:1 (create-vm memory [1])})))

(defn part-2 []
  (let [memory (load-and-parse-input)]
    (run {:1 (create-vm memory [2])})))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
