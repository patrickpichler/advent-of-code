(ns day13
  (:require [clojure.string :as string]
            [clojure.algo.generic.math-functions :refer [sgn]]

            [util.intcode :as intcode]))

(def debug-active false)

(defmacro debug [& args]
  `(if ~debug-active
     (println "DEBUG:" ~@args)))

(defn load-input []
  ; (slurp "inputs/day9/input1"))
  (slurp "inputs/day13/input1"))
  ; (slurp "sample"))

(defn create-screen-state [state previous display score]
  (intcode/create-computer-run-state state [previous display score]))

(defn id->tile [id]
  (case id
    0 :EMPTY
    1 :WALL
    2 :BLOCK
    3 :PADDLE
    4 :BALL))

(defn run-screen [previous display score input]
  (if (empty? input)
    (create-screen-state :WAITING_FOR_INPUT previous display score)
    (let [[px py id & r] input]
      (if (> px -1)
        (let [tile (id->tile id)
              updated-display (assoc display [px py] tile)]
          (recur (cons updated-display previous)
                 updated-display
                 score
                 r))
        (recur previous
               display
               id
               r)))))

(defn run-screen-with-args [[previous display score] input args]
  (run-screen previous display score input))

(defn create-ai-state [state ball paddle action]
  (intcode/create-computer-run-state state [ball paddle] [action]))

(defn run-ai [ball paddle input action]
  (if (empty? input)
    (create-ai-state :WAITING_FOR_INPUT ball paddle (or action 0))
    (let [[px py id & r] input]
      (if (= px -1)
        (recur ball paddle r action)
        (let [tile-type (id->tile id)
              updated-ball (if (= tile-type :BALL) px ball)
              updated-paddle (if (= tile-type :PADDLE) px paddle)]
          (if (or (nil? updated-paddle) (nil? updated-ball))
            (recur updated-ball updated-paddle r action)
            (recur updated-ball updated-paddle r (sgn (- updated-ball updated-paddle)))))))))

(defn run-ai-with-args [[ball paddle] input args]
  (run-ai ball paddle input 0))

(defn part-1 []
  (let [memory (intcode/parse-intcode-memory (load-input))
        vms (intcode/run {:1 (intcode/create-intcode-vm memory [])
                          :screen (intcode/create-vm-state run-screen-with-args [{}])}

                         {:1 [:screen]})

        [_ _ [_ display]] (vms :screen)]
    (count (filter #(= (second %) :BLOCK) display))))

(defn reduce-min-max-positions [[min-x min-y max-x max-y] [px py]]
  (vector (min min-x px) (min min-y py) (max max-x px) (max max-y py)))

(def INITIAL_MIN_MAX_REDUCER_VALUE
  [Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE])

(def ANSI-RESET "\u001B[0m")
(def ANSI-BLACK "\u001B[30m")
(def ANSI-RED "\u001B[31m")
(def ANSI-GREEN "\u001B[32m")
(def ANSI-YELLOW "\u001B[33m")
(def ANSI-BLUE "\u001B[34m")
(def ANSI-PURPLE "\u001B[35m")
(def ANSI-CYAN "\u001B[36m")
(def ANSI-WHITE "\u001B[37m")
(def ANSI-CLEAR "\033[H\033[2J")

(defn ansi-color-str [s color]
  (str color s ANSI-RESET))

(defn tile->pixel [d]
  (case d
    :EMPTY "\u2591"
    :WALL "\u2588"
    :BLOCK (ansi-color-str "\u2588" ANSI-RED)
    :PADDLE "\u254c"
    :BALL (ansi-color-str "\u2588" ANSI-PURPLE)))

(defn render-display! [display]
  (let [[min-x min-y max-x max-y]
        (reduce reduce-min-max-positions INITIAL_MIN_MAX_REDUCER_VALUE
                (keys display))
        start-x (min min-x 0)
        start-y (min min-y 0)
        width (- max-x start-x)
        height (- max-y start-y)]
    (println ANSI-CLEAR)
    (->> (for [y (range start-y (+ max-y 1))
               x (range start-x (+ max-x 2))]
           [x y])
         (map #(display % :EMPTY))
         (map tile->pixel)
         (partition-all (+ width 2))
         (map string/join)
         (run! println)
         (doall))
    (flush)
    (Thread/sleep 100)))

(defn part-2 []
  (let [memory (intcode/parse-intcode-memory (load-input))
        vms (intcode/run {:1 (intcode/create-intcode-vm (assoc memory 0 2))
                          :screen (intcode/create-vm-state run-screen-with-args [{}])
                          :ai (intcode/create-vm-state run-ai-with-args [])}

                         {:1 [:screen :ai]
                          :ai [:1]})

        [_ _ [previous current score]] (vms :screen)]
    score))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
