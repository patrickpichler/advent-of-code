(ns day12
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn load-input []
  (slurp "inputs/day12/input1"))
  ; (slurp "sample"))
  ; (slurp "sample2"))

(defn get-values-from-line [line]
  (let [[_ x y z] (re-matches #"<x=(-?\d*), y=(-?\d*), z=(-?\d*)>" line)]
    (mapv #(Integer/parseInt %) [x y z])))

(defn parse-input []
  (->> (load-input)
       string/split-lines
       (map-indexed #(vector (str %1) [(get-values-from-line %2) [0 0 0]]))
       (into {})))

(defn get-velocity-change [n1 n2]
  (cond (> n1 n2) -1
        (< n1 n2) +1
        :else 0))

(comment
  (get-velocity-change 3 5)
  (get-velocity-change 5 3))

(defn invert-change [change]
  (mapv - change))

(defn calculate-velocity-change [k1 [p1 _] k2 [p2 _]]
  (let [change1 (map get-velocity-change p1 p2)
        change2 (invert-change change1)]
    [[k1 k2 change1] [k2 k1 change2]]))

(defn merge-vectors [v1 v2]
  (mapv + v1 v2))

(defn get-velocity-changes [moons-map]
  (->> moons-map
       (map (fn [[k [p v]]]
              (vector k (->> (vals moons-map)
                             (map first)
                             (map #(map get-velocity-change p %))
                             (reduce merge-vectors [0 0 0])))))
       (into {})))

(defn sum-vector-absolute-vals [v]
  (reduce + (map #(Math/abs %) v)))

(defn calculate-total-energy-for-moon [[_ [pos vel]]]
  (let [sum-pos (sum-vector-absolute-vals pos)
        sum-vel (sum-vector-absolute-vals vel)]
    (* sum-pos sum-vel)))

(defn apply-velocity-changes-and-move [moons-map velocity-changes]
  (->> moons-map
       (map (fn [[k [pos velocity]]]
              (let [velocity-change (velocity-changes k)
                    new-velocity (merge-vectors velocity velocity-change)
                    new-pos (merge-vectors pos new-velocity)]
                [k [new-pos new-velocity]])))
       (into {})))

(defn step-moons [moons]
  (let [changes (get-velocity-changes moons)
        updated-moons (apply-velocity-changes-and-move moons changes)]
    updated-moons))

(defn part-1 []
  (let [input (parse-input)]
    (->> (iterate step-moons input)
         (drop 1000)
         (map #(reduce + (map calculate-total-energy-for-moon %)))
         first)))

(defn gcd [a b]
  (loop [x a
         y b]
    (if (= y 0) x
        (recur y (rem x y)))))

(defn lcm
  ([a b]
   (if (or (= a 0) (= b 0)) 0
       (/ (* a b) (gcd a b))))
  ([a b & c]
   (reduce lcm (lcm a b) c)))

(defn detect-loop [[f c] p]
  (if (= f p) (reduced (inc c))
      [(or f p) (inc c)]))

(defn part-2 []
  (let [input (parse-input)]
    (->>
     (range 0 3)
     (map (fn [n] (->> (iterate step-moons input)
                       (map vals)
                       (map #(map first %))
                       (map #(apply map vector %))
                       (map #(nth % n))
                       (reduce detect-loop [nil 0]))))
     (apply lcm))))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))

