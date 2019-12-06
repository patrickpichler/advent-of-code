(ns day6
  (:require [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day6/input1"))

(defn parse-orbit [s]
  (string/split s #"\)"))

(defn parse-input-1 [input]
  (->> input
       string/split-lines
       (map parse-orbit)
       (group-by first)
       (map (fn [[k v]] (vector k (map last v))))
       (into {})))

(defn get-orbit-count [orbit-map]
  (loop [orbit-count {}
         object-queue '(["COM" 0])]
    (let [[object object-count] (first object-queue)
          children (orbit-map object)]
      (if (nil? object)
        orbit-count
        (recur (assoc orbit-count object object-count)
               (concat (map #(vector % (inc object-count)) children) (rest object-queue)))))))

(defn part-1 []
  (let [input (load-input)
        orbit-map (parse-input-1 input)]
    (->> orbit-map
         get-orbit-count
         (map last)
         (reduce +))))

(defn parse-input-2 [input]
  (->> input
       string/split-lines
       (map parse-orbit)
       (map (fn [[k v]] (vector v k)))
       (into {})))

(defn find-orbit-path [orbit-parent-map object]
  (loop [current-object object
         path '()]
    (if (nil? current-object)
      path
      (recur (orbit-parent-map current-object)
             (cons current-object path)))))

(defn find-common-parent [p1 p2]
  (->> (map vector p1 p2)
       (take-while (fn [[a b]] (= a b)))
       last
       first))

(defn count-path-from-til-end [path object]
  (->> path
       (drop-while #(not= object %))
       count
       dec))

(defn part-2 []
  (let [input (load-input)
        parent-map (parse-input-2 input)
        my-orbit (parent-map "YOU")
        santa-orbit (parent-map "SAN")
        my-orbit-path (find-orbit-path parent-map my-orbit)
        santa-orbit-path (find-orbit-path parent-map santa-orbit)
        common-parent (find-common-parent santa-orbit-path my-orbit-path)]
    (+ (count-path-from-til-end santa-orbit-path common-parent)
       (count-path-from-til-end my-orbit-path common-parent))))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))

(comment

  (->> [[:a 1] [:b 2] [:a 3]]
       (group-by first)
       (map (fn [[k v]] (vector k (map last v))))
       (into {})))
