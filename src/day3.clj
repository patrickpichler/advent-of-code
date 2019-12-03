(ns day3
  (:require [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day3/input1"))

(defn translate-instruction-to-point [[x y] instr]
  (let [dir (.substring instr 0 1)
        number (Integer/parseInt (.substring instr 1))]
    (case dir
      "R" [(+ x number) y]
      "L" [(- x number) y]
      "U" [x (- y number)]
      "D" [x (+ y number)])))

(defn parse-lines [instructions]
  (loop [current-pos [0 0]
         instrs instructions
         parsed-lines []]
    (if (seq instrs)
      (let [instr (first instrs)
            new-pos (translate-instruction-to-point current-pos instr)
            line [current-pos new-pos]]
        (recur new-pos
               (rest instrs)
               (cons line parsed-lines)))
      (vec (reverse parsed-lines)))))

(defn intersect? [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (cond
    (and (or (< x1 x4 x2) (> x1 x4 x2))
         (or (< y3 y1 y4) (> y3 y1 y4))) [x3 y1]
    (and (or (< x3 x1 x4) (> x3 x1 x4))
         (or (< y1 y3 y2) (> y1 y3 y2))) [x1 y3]
    :else nil))

(defn find-intersections [[w1 w2]]
  (->>
   (for [s1 w1
         s2 w2]
     [s1 s2])
   (keep (fn [[w1 w2 :as p]]
           (let [intersect (intersect? w1 w2)]
             (if intersect
               (vector p intersect)))))
   (into [])))

(defn parse-input [input]
  (->>
   input
   (string/split-lines)
   (map #(string/split %1 #","))
   (map parse-lines)
   (into [])))

(defn part-1 [input]
  (->> (parse-input input)
       find-intersections
       (map last)
       (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
       sort
       first))

(defn length [[x1 y1] [x2 y2]]
  (Math/abs
   (if (= x1 x2)
     (- y2 y1)
     (- x2 x1))))

(defn length-line [[p1 p2]]
  (length p1 p2))

(defn length-til-point [[p1 _] p2]
  (length p1 p2))

(defn update-distance-map [distances line intersections current-distance]
  (if (seq intersections)
    (->> intersections
         (map #(vector %1 (length-til-point line %1)))
         (reduce (fn [m [i l]]
                   (update m i #(+ %1 (+ current-distance l)))) distances))
    distances))

(defn find-distances-for-wire [wire distances intersections-map]
  (loop [distances distances
         lines wire
         current-distance 0]

    (if (seq lines)
      (let [current-line (first lines)
            intersections (intersections-map current-line)
            updated-distances (update-distance-map distances current-line intersections current-distance)
            l (length-line current-line)]
        (recur updated-distances
               (rest lines)
               (+ current-distance l)))
      distances)))

(defn part-2 [input]
  (let [wires (parse-input input)
        intersections (find-intersections wires)
        intersections-map (->> (find-intersections (parse-input input))
                               (mapcat (fn [[[l1 l2] i]]
                                         (vector
                                          (vector l1 i)
                                          (vector l2 i))))
                               (group-by first)
                               (map (fn [[k v]] (vector k (mapv last v))))
                               (into {}))
        distances (->> intersections
                       (map #(vector (last %) 0))
                       (into {}))]
    (->> wires
         (map #(find-distances-for-wire %1 distances intersections-map))
         (apply merge-with +)
         (map last)
         sort
         first)))

(defn -main [& args]
  (let [input (load-input)]
    (println (part-1 input))
    (println (part-2 input))))
