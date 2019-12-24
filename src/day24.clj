(ns day24
  (:require
   [clojure.string :as string]))

(defn load-input []
  (slurp "inputs/day24/input1"))

(defn parse-line [row line]
  (->> line
       (map-indexed #(vector [%1 row] %2))
       (filter #(= (second %) \#))))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map-indexed parse-line)
       (mapcat identity)
       (map first)
       (into #{})))

(defn render-map [m]
  (->> (for [y (range 0 5)
             x (range 0 5)]
         [x y])
       (map #(cond
               (= % [2 2]) \?
               (contains? m %) \#
               :else \.))
       (partition-all 5)
       (map string/join)
       (run! println)))

(defn translate [dir [x y]]
  (case dir
    :LEFT [(dec x) y]
    :RIGHT [(inc x) y]
    :UP [x (dec y)]
    :DOWN [x (inc y)]))

(def directions [:LEFT :RIGHT :UP :DOWN])

(defn count-neighbors [m pos]
  (->> directions
       (map #(translate % pos))
       (filter #(contains? m %))
       count))

(defn evolve [m]
  (->> (for [y (range 0 5)
             x (range 0 5)]
         [x y])
       (map #(vector % (contains? m %) (count-neighbors m %)))
       (filter (fn [[pos bug neighbor-count]]
                 (or
                  (and bug (= neighbor-count 1))
                  (and (not bug) (or (= neighbor-count 1) (= neighbor-count 2))))))
       (map first)
       (into #{})))

(defn find-previous-layout [layouts m]
  (if (contains? layouts m) (reduced m)
      (conj layouts m)))

(defn pos->biodiversity [[x y]]
  (Math/pow 2 (+ (* y 5) x)))

(defn calculate-biodiversity-rating [m]
  (->> m
       (map pos->biodiversity)
       (reduce +)
       int))

(defn part-1 []
  (let [input (load-input)
        m (parse-input input)]
    (->> (iterate evolve m)
         (reduce find-previous-layout #{})
         calculate-biodiversity-rating)))

(def right-side (into [] (for [y (range 0 5)] [4 y])))
(def left-side (into [] (for [y (range 0 5)] [0 y])))
(def up-side (into [] (for [x (range 0 5)] [x 0])))
(def down-side (into [] (for [x (range 0 5)] [x 4])))

(defn to-neighbors-recursive-inwards [level dir f]
  (let [next-level (f level)]
    (case dir
      :LEFT (map #(vector % next-level) right-side)
      :RIGHT (map #(vector % next-level) left-side)
      :UP (map #(vector % next-level) down-side)
      :DOWN (map #(vector % next-level) up-side))))

(defn to-neighbors-recursive-outwards [level dir f]
  (let [next-level (f level)]
    (case dir
      :LEFT [[[1 2] next-level]]
      :RIGHT [[[3 2] next-level]]
      :UP [[[2 1] next-level]]
      :DOWN [[[2 3] next-level]])))

(defn inflate-recursive-neighbors [[x y :as pos] level dir]
  (cond
    (= [x y] [2 2]) (to-neighbors-recursive-inwards level dir inc)
    (or (= x -1) (= x 5)) (to-neighbors-recursive-outwards level dir dec)
    (or (= y -1) (= y 5)) (to-neighbors-recursive-outwards level dir dec)
    :else [[pos level]]))

(defn count-neighbors-2 [m [pos level]]
  (->> directions
       (map #(vector (translate % pos) %))
       (mapcat (fn [[p dir]]
                 (let [inflated (inflate-recursive-neighbors p level dir)]
                   inflated)))
       (distinct)
       (filter #(contains? m %))
       count))

(defn render-multi [m]
  (->> m
       (group-by second)
       (sort-by first)
       (map (fn [[k v]]
              (vector k (map first v))))
       (run! (fn [[k v]]
               (println "--- " k)
               (render-map (into #{} v))
               (println)))))

(defn evolve-2 [[m min-level max-level]]
  (let [new-m (->> (for [l (range min-level (inc max-level))
                         y (range 0 5)
                         x (range 0 5)]
                     [[x y] l])
                   (filter #(not= [2 2] (first %)))
                   (map #(vector % (contains? m %) (count-neighbors-2 m %)))
                   (filter (fn [[p bug neighbor-count]]
                             (or
                              (and bug (= neighbor-count 1))
                              (and (not bug) (or (= neighbor-count 1) (= neighbor-count 2))))))
                   (map first)
                   (into #{}))
        new-max-level (inc (reduce max 0 (map second new-m)))
        new-min-level (dec (reduce min 0 (map second new-m)))]
    [new-m new-min-level new-max-level]))

(defn part-2 []
  (let [input (load-input)
        m (vector (into #{} (map #(vector % 0)) (parse-input input)) -1 1)]
    (->> (iterate evolve-2 m)
         (take 201)
         last
         first
         count)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
