(ns day20
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [clojure.data.priority-map :as pm]))

(set! *warn-on-reflection* true)

(defn load-input []
  (slurp "inputs/day20/input1"))

(defn map-char [c]
  (cond
    (Character/isLetter ^char c) c
    (= c \.) \.
    :else \#))

(defn parse-line [row line]
  (->> line
       (map-indexed #(vector (vector %1 row) (map-char %2)))))

(defn translate-pos [dir [x y]]
  (case dir
    :LEFT [(dec x) y]
    :RIGHT [(inc x) y]
    :UP [x (dec y)]
    :DOWN [x (inc y)]))

(def directions [:LEFT :RIGHT :UP :DOWN])

(defn is-part-of-portal? [c]
  (and (not (nil? c))
       (Character/isLetter ^char c)))

(defn is-free? [c]
  (= c \.))

(defn find-second-part [pos m]
  (->> directions
       (map #(vector % (translate-pos % pos)))
       (keep (fn [[dir p]]
               (let [c (m p \#)]
                 (if (is-part-of-portal? c)
                   [dir p c]
                   nil))))
       first))

(defn find-portal-position [m dir first-pos second-pos]
  (case dir
    :UP (if (is-free? (m (translate-pos :UP second-pos) \#))
          (translate-pos :UP second-pos)
          (translate-pos :DOWN first-pos))
    :DOWN (if (is-free? (m (translate-pos :DOWN second-pos) \#))
            (translate-pos :DOWN second-pos)
            (translate-pos :UP first-pos))

    :LEFT (if (is-free? (m (translate-pos :LEFT second-pos) \#))
            (translate-pos :LEFT second-pos)
            (translate-pos :RIGHT first-pos))

    :RIGHT (if (is-free? (m (translate-pos :RIGHT second-pos) \#))
             (translate-pos :RIGHT second-pos)
             (translate-pos :LEFT first-pos))))

(defn get-portal-name [dir c pos second-c second-pos]
  (case dir
    :LEFT [(str second-c c) second-pos]
    :RIGHT [(str c second-c) pos]
    :UP [(str second-c c) second-pos]
    :DOWN [(str c second-c) pos]))

(defn parse-input []
  (let [raw-map (->> (load-input)
                     string/split-lines
                     (map-indexed #(parse-line %1 %2))
                     (mapcat identity)
                     (into {}))]
    (->> raw-map
         (reduce (fn [[result entry-point-set] [pos c]]
                   (if (not (is-part-of-portal? c)) [result entry-point-set]

                       (let [[dir second-pos second-c] (find-second-part pos raw-map)
                             [portal portal-init-pos :as p] (get-portal-name dir c pos second-c second-pos)]

                         (if (contains? entry-point-set p) [result entry-point-set]
                             (let [portal-pos (find-portal-position raw-map dir pos second-pos)]
                               [(conj result [portal-pos [portal portal-pos]] [pos \#] [second-pos \#])
                                (conj entry-point-set p)])))))

                 ['() #{}])
         first
         (into {})
         (merge raw-map))))

(defn render-map [m]
  (let [[max-x max-y] (reduce
                       (fn [[max-x max-y] [[x y] _]]
                         [(max max-x x) (max max-y y)])
                       [0 0] m)]
    (->> (for [y (range 0 max-y)
               x (range 0 max-x)]
           [x y])
         (map #(or (m %) \.))
         (map #(if (vector? %)
                 (first %)
                 (str %)))
         (map first)
         (partition-all max-x)
         (map string/join)
         (string/join "\n"))))

(defn find-portal-positions [m]
  (map first (filter (comp vector? second) m)))

(defn is-traversable? [m pos]
  (not= (m pos \#) \#))

(defn find-distance [m p1 p2]
  (loop [nodes (list [p1 0])
         visited-nodes #{}]
    (if (empty? nodes) -1
        (let [[[node distance] & r] nodes]
          (if (= node p2) distance
              (let [neighbors (->> directions
                                   (map #(translate-pos % node))
                                   (filter #(not (contains? visited-nodes %)))
                                   (filter (partial is-traversable? m))
                                   (map #(vector % (inc distance)))
                                   doall)]
                (recur (doall (concat neighbors r))
                       (conj visited-nodes node))))))))

(defn preprocess-map [m]
  (let [portals (find-portal-positions m)
        portal-combos (combo/combinations portals 2)]
    (->> portal-combos
         (map (fn [[pos1 pos2]]
                (let [[n1 :as portal1] (m pos1)
                      [n2 :as portal2] (m pos2)]
                  (if (= n1 n2)
                    (vector [portal1 portal2] 1)
                    (vector [portal1 portal2] (find-distance m pos1 pos2))))))
         (filter #(not= (second %) -1))
         (mapcat (fn [[[p1 p2] result]]
                   [[p1 [p2 result]] [p2 [p1 result]]]))
         (group-by first)
         (map (fn [[k v]] (vector k (into {} (map second v)))))
         (into {}))))

(defn find-shortests-path [m paths start-pos end-pos]
  (loop [nodes (pm/priority-map start-pos 0)
         visited-nodes #{}]
    (if (empty? nodes) -1
        (let [[pos distance :as node] (peek nodes)
              r (pop nodes)]
          (if (= pos end-pos) distance
              (let [updated-visited-nodes (conj visited-nodes node)
                    neighbors (->> (paths pos [])
                                   (filter #(not (contains? visited-nodes (first %))))
                                   (map (fn [[k v]] [k (+ distance v)]))
                                   (into {}))]
                (recur (merge-with min r neighbors)
                       updated-visited-nodes)))))))

(defn find-position-of-portal [m portal]
  (->> m
       (filter (comp vector? second))
       (filter #(= (first (second %)) portal))
       first
       second))

(defn part-1 []
  (let [m (parse-input)
        paths (preprocess-map m)
        start-pos (find-position-of-portal m "AA")
        end-pos (find-position-of-portal m "ZZ")]
    (find-shortests-path m paths start-pos end-pos)))

(defn label-portal [max-x max-y [x y]]
  (if (or (= x 2) (= (- max-x 2) x)
          (= y 2) (= (- max-y 2) y)) :OUTER :INNER))

(defn label-nodes [m paths]
  (let [[max-x max-y] (reduce (fn [[width height] [x y]]
                                [(max width x) (max height y)]) (keys m))
        portals (find-portal-positions m)]
    (->> portals
         (map #(vector (m %) (label-portal max-x max-y %)))
         (into {}))))

(defn find-shortests-path-2 [m paths labled-nodes portal-map start-pos end-pos]
  (loop [nodes (pm/priority-map [start-pos 0] 0)
         visited-nodes #{}]
    (if (empty? nodes) -1
        (let [[[[portal-name :as pos] level :as id] distance :as node] (peek nodes)
              r (pop nodes)]
          (cond
            (contains? visited-nodes id) (recur r visited-nodes)
            (= pos end-pos) distance
            :else (let [updated-visited-nodes (conj visited-nodes id)
                        neighbors (->> (paths pos [])
                                       (filter #(not (contains? visited-nodes [(first %) level])))
                                       (keep (fn [[[n :as k] v]]
                                               (let [label (labled-nodes k)
                                                     other-portal (portal-map k)]
                                                 (cond
                                                   (= n portal-name) nil
                                                   (and (= k end-pos) (= level 0)) [[k 0] (+ distance v)]
                                                   (and (= k end-pos)) nil
                                                   (= label :INNER) [[other-portal (inc level)] (+ distance v 1)]
                                                   (and (= label :OUTER) (= level 0)) nil
                                                   (= label :OUTER) [[other-portal (dec level)] (+ distance v 1)]))))
                                       (into {}))]
                    (recur (merge-with min r neighbors)
                           updated-visited-nodes)))))))

(defn get-portal-map [m]
  (->> (vals m) 
       (filter vector?)
       (group-by first)
       (filter #(= 2 (count (second %))))
       (mapcat (fn [[_ [p1 p2]]]
              [[p2 p1] [p1 p2]]))
       (into {})))

(defn part-2 []
  (let [m (parse-input)
        paths (preprocess-map m)
        labled-nodes (label-nodes m paths)
        start-pos (find-position-of-portal m "AA")
        end-pos (find-position-of-portal m "ZZ")
        portal-map (get-portal-map m)]
    (find-shortests-path-2 m paths labled-nodes portal-map start-pos end-pos)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
