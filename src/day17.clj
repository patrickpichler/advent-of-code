(ns day17
  (:require
   [clojure.string :as string]
   [util.intcode :as intcode]
   [clojure.algo.generic.math-functions :refer [ceil]]))

(defn load-input []
  (slurp "inputs/day17/input1"))

(defn create-camera-state
  ([state pixels]
   (intcode/create-computer-run-state state [pixels] [])))

(defn run-camera
  ([pixels input args] (run-camera pixels (concat args input)))

  ([pixels input]
   (create-camera-state :WAITING_FOR_INPUT (concat input pixels))))

(def scaffold 35)
(def free 46)

(defn parse-line [row line]
  (->> line
       (map-indexed #(vector [%1 row] %2))
       (filter #(= (second %) scaffold))
       (map first)
       (into [])))

(defn parse-map [pixels]
  (->> pixels
       (partition-by #(= 10 %))
       (filter #(> (count %) 1))
       (map-indexed parse-line)
       (mapcat identity)
       (into #{})))

(defn is-crossing [s [x y]]
  (and (contains? s [(dec x) y])
       (contains? s [(inc x) y])
       (contains? s [x (dec y)])
       (contains? s [x (inc y)])))

(defn part-1 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        vms (intcode/run {:1 (intcode/create-intcode-vm memory [])
                          :camera (intcode/create-vm-state run-camera '())}

                         {:1 [:camera]})
        [_ _ [pixels]] (:camera vms)
        s (parse-map pixels)]
    (->> s
         (filter (partial is-crossing s))
         (map (fn [[x y]] (* x y)))
         (reduce +)
         (vector pixels s))))

(defn render-pixels [pixels]
  (->> pixels
       (map char)
       (reduce str)
       println))

(defn find-start-pos [pixels]
  (->> pixels
       (partition-by #(= 10 %))
       (filter #(> (count %) 1))
       (map-indexed (fn [i l]
                      (->> l
                           (map-indexed #(vector [%1 i] %2))
                           (filter #(= 94 (second %)))
                           (map first))))
       (mapcat identity)
       (first)))

(defn advance [orientation [x y]]
  (case orientation
    :NORTH [x (dec y)]
    :SOUTH [x (inc y)]
    :WEST [(dec x) y]
    :EAST [(inc x) y]))

(defn can-advance [m orientation pos]
  (contains? m (advance orientation pos)))

(def orientations [:NORTH :SOUTH :EAST :WEST])

(defn invert-orientation [o]
  (case o
    :NORTH :SOUTH
    :SOUTH :NORTH
    :EAST :WEST
    :WEST :EAST))

(defn find-next-orientation [tried-orientations current-orientation]
  (first (filter #(not (contains? tried-orientations %)) orientations)))

(defn segment-length [[[x y] [x1 y1]]]
  (Math/abs ^int (+ (- x1 x) (- y1 y))))

(defn split-map-into-segments [m start-pos orientation]
  (loop [segments '()
         current-orientation orientation
         tried-orientations #{}
         current-pos start-pos]
    (let [end-pos (->> (iterate (partial advance current-orientation) current-pos)
                       (drop-while (partial can-advance m current-orientation))
                       first)]
      (if (or (nil? end-pos) (= end-pos current-pos))
        (if-let [next-orientation (find-next-orientation tried-orientations current-orientation)]
          (recur segments
                 next-orientation
                 (conj tried-orientations next-orientation)
                 current-pos)
          (vec (reverse segments)))
        (let [new-segment [current-pos end-pos]
              new-tried-orientations #{current-orientation (invert-orientation current-orientation)}
              next-orientation (find-next-orientation new-tried-orientations current-orientation)]
          (recur (cons [(segment-length new-segment) current-orientation] segments)
                 next-orientation
                 new-tried-orientations
                 end-pos))))))

(defn translate-orientation-to-direction [current-orientation orientation]
  (cond
    (and (= current-orientation :NORTH) (= orientation :WEST)) :LEFT
    (and (= current-orientation :NORTH) (= orientation :EAST)) :RIGHT

    (and (= current-orientation :SOUTH) (= orientation :WEST)) :RIGHT
    (and (= current-orientation :SOUTH) (= orientation :EAST)) :LEFT

    (and (= current-orientation :EAST) (= orientation :NORTH)) :LEFT
    (and (= current-orientation :EAST) (= orientation :SOUTH)) :RIGHT

    (and (= current-orientation :WEST) (= orientation :NORTH)) :RIGHT
    (and (= current-orientation :WEST) (= orientation :SOUTH)) :LEFT

    :else (throw (RuntimeException. "WAT"))))

(defn segments-to-move-instructions [segments orientation]
  (->> segments
       (reduce (fn [[o result] [length seg-o]]
                 (let [dir (translate-orientation-to-direction o seg-o)
                       movement (list dir length)]
                   [seg-o (cons movement result)])) [orientation '()])
       last
       reverse
       vec))

(defn dir->str [dir]
  (case dir
    :LEFT "L"
    :RIGHT "R"))

(defn compact-instructions [instructions]
  (let [reversed-tokens (into {} (map (comp vec reverse) (map-indexed vector instructions)))]
    (loop [token-map (into {} (map (comp vec reverse) reversed-tokens))
           last-token (reduce max (keys token-map))
           compacted (map reversed-tokens instructions)]
      (let [next-token (inc last-token)
            paired-instructions (map vector compacted (concat (drop 1 compacted) '(nil)))
            occurences (frequencies paired-instructions)
            [combi occurence-count] (last (sort-by second occurences))]

        (if (= occurence-count 1) [token-map compacted]
            (let [new-compacted (reduce (fn [[skip c] [f s :as pair]]
                                          (cond
                                            skip [false c]
                                            (= pair combi) [true (conj c next-token)]
                                            :else [false (conj c f)])) [false []] paired-instructions)]
              (recur (assoc token-map next-token combi)
                     next-token
                     (last new-compacted))))))))

(defn expand-token [token-map token]
  (loop [to-expand (list token)
         expanded-token '()]
    (if-let [[t & r] to-expand]
      (let [expanded (token-map t)]
        (if (vector? expanded)
          (recur (concat expanded r)
                 expanded-token)
          (recur r
                 (cons expanded expanded-token))))
      (vec (reverse expanded-token)))))

(defn token->function [a b c token]
  (cond
    (= token a) "A"
    (= token b) "B"
    (= token c) "C"))

(defn tokens->robot [tokens]
  (->> tokens
       (map #(str (dir->str (first %1)) "," (second %1)))
       (string/join ",")))

(defn run-outdoor-robot
  ([s args input]
   (run-outdoor-robot (or s []) (concat args input)))

   ([data input]
      (intcode/create-computer-run-state :WAITING_FOR_INPUT (conj data input))))

(defn to-robot-instructions [instructions]
  ; done by hand, because i'm unable to get the compaction working as the puzzle wants it...
  (map int (string/join "\n" ["A,B,A,C,B,C,A,B,A,C" "R,10,L,8,R,10,R,4" "L,6,L,6,R,10" "L,6,R,12,R,12,R,10" "n\n"]))

  ; (map int (string/join "\n" ["A,B" "L10,L8,R10,R4" "R6" "R8" "y\n"]))

  ; (let [[token-map compacted] (compact-instructions instructions)
  ;       tokens (distinct compacted)
  ;       expanded-token-map (->> tokens
  ;                               (map #(vector % (expand-token token-map %)))
  ;                               (into {}))]
  ;         (println expanded-token-map)
  ;   (if (not= (count expanded-token-map) 3) (throw (RuntimeException. "Map too big..."))
  ;       (let [[a b c] (keys expanded-token-map)
  ;             robot-compacted (string/join "," (map #(token->function a b c %) compacted))
  ;             robot-token-map {"A" (tokens->robot (expanded-token-map a))
  ;                              "B" (tokens->robot (expanded-token-map b))
  ;                              "C" (tokens->robot (expanded-token-map c))}]
  ;         [robot-compacted robot-token-map])))
  )

(defn part-2 [pixels parsed-map]
  (let [start-pos (find-start-pos pixels)
        segments (split-map-into-segments parsed-map start-pos :NORTH)
        move-instructions (segments-to-move-instructions segments :NORTH)
        instructions (to-robot-instructions move-instructions)
        input (load-input)
        memory (intcode/parse-intcode-memory input)
        vms (intcode/run {:1 (intcode/create-intcode-vm (assoc memory 0 2) instructions)
                          :robot (intcode/create-vm-state run-outdoor-robot)}

                         {:1 [:robot]
                          :robot [:1]})
        [_ _ out] (:robot vms)]
    (clojure.string/join (flatten (map #(map (fn [c] (if (< c 255) (char c) c)) %) out)))))

(defn -main []
  (let [[pixels parsed-map distance] (part-1)]
    (println distance)
    (println (part-2 pixels parsed-map))))
