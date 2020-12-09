(ns day9
  (:require [util :refer [load-lines]]))

(defn is-valid-number? [number numbers]
  (first (for [x (range 0 (count numbers))
               y (range 0 (count numbers))
               :let [sum (+ (nth numbers x) (nth numbers y))]
               :when (and (not= x y) (= number sum))]
           true)))

(comment
  (is-valid-number? 4 [2 2 3])
  (is-valid-number? 5 [2 2 3])
  (is-valid-number? 6 [2 2 3]))

(def preamble 25)

(defn solve-part-1 [input]
  (let [window (partition preamble 1 input)]
    (ffirst (filter (fn [[i w]] (not (is-valid-number? i w)))
                    (map vector (drop preamble input) window)))))

(defn find-min-max [numbers lower upper]
  (let [f (nth numbers lower)
        sub-seq (take (inc (- upper lower))
                      (drop lower numbers))]
    (reduce (fn [[mi ma :as m] n]
              (vector (min mi n) (max ma n)))
            [f f] sub-seq)))

(defn solve-part-2 [number [f s & r :as numbers]]
  (loop [lower 0
         upper 1
         sum (+ f s)]
    (cond
      (= sum number) (reduce +
                             (find-min-max numbers lower upper))
      (> sum number) (recur (inc lower)
                            upper
                            (- sum (nth numbers lower)))
      :else (recur lower
                   (inc upper)
                   (+ sum (nth numbers (inc upper)))))))

(comment
  (def sample [35
               20
               15
               25
               47
               40
               62
               55
               65
               95
               102
               117
               150
               182
               127
               219
               299
               277
               309
               576])
  (solve-part-2 127 sample))

(defn -main [& args]
  (let [input (mapv #(Long/parseLong %) (load-lines 9))
        number (solve-part-1 input)]
    (println number)
    (println (solve-part-2 number input))))
