(ns day3
  (:require [clojure.string :as string]))

(defn parse-input [input]
  (to-array-2d (string/split-lines input)))

(defn count-trees [arr [slope-x slope-y]]
  (let [len-y (alength arr)
        len-x (alength (aget arr 0))]
    (loop  [curr-x 0
            curr-y 0
            trees 0]
      (if (>= curr-y len-y)
        trees
        (recur (+ curr-x slope-x)
               (+ curr-y slope-y)
               (if (= (aget arr curr-y (mod curr-x len-x)) \#)
                 (inc trees)
                 trees))))))

(comment
  (>= 324 323)

  (mod 3 3)
  (def i (parse-input (slurp "inputs/day3")))
  (let [arr (to-array-2d [[1 2 3] [4 5 6]])]
    (alength (aget arr 0)))

  (count-trees i [3 1])

  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
           (map #(count-trees i %))
           (reduce *))
  )

(defn -main [& args]
  (let [input (slurp "inputs/day3")
        arr (parse-input input)]
    (println (count-trees arr [3 1]))
    (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
         (map #(count-trees arr %))
         (reduce *)
         println)))
