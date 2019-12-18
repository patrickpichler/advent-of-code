(ns day18
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :as pm]
            [flatland.ordered.set :refer [ordered-set]]))

; (set! *warn-on-reflection* true)

(defn load-input []
  (slurp "inputs/day18/input1"))

(defn parse-line [row line]
  (->> line
       (map-indexed #(vector [%1 row] %2))))

(defn parse-map [input]
  (->> input
       string/split-lines
       (map-indexed #(parse-line %1 %2))
       (mapcat identity)
       (into {})))

(defn find-start-positions [m]
  (->> m
       (filter #(= \@ (second %)))
       (map first)))

(defn find-keys [m]
  (->> m
       (filter #(Character/isLowerCase ^char (second %)))
       (map first)
       (into [])))

(defn find-doors [m]
  (->> m
       (filter #(Character/isUpperCase ^char (second %)))
       (map first)
       (into [])))

(defn translate-pos [[x y] ^long action]
  (case action
    1 [x (dec y)]
    2 [x (inc y)]
    3 [(dec x) y]
    4 [(inc x) y]
    [x y]))

(defn is-door? [c]
  (Character/isUpperCase ^char c))

(defn is-key? [c]
  (Character/isLowerCase ^char c))

(defn is-wall? [c]
  (= c \#))

(defn add-key [mask k]
  (let [pos (- (int k) (int \a))]
    (bit-set mask pos)))

(defn has-key [mask k]
  (let [pos (- (int k) (int \a))]
    (bit-test mask pos)))

(defn find-neighbors [m visited pos]
  (let [neighbors (into []
                        (comp (map #(translate-pos pos %))
                              (filter #(not (contains? visited %)))
                              (filter #(let [c (m %)]
                                         (and (not (nil? c))
                                              (not (is-wall? c)))))
                              (map #(if (is-door? (m %)) (vector % #{(Character/toLowerCase ^char (m %))})
                                        (vector % #{}))))
                        (range 0 5))]
    neighbors))

(defn find-paths [m start-pos target]
  (loop [nodes (list [start-pos 0 0])
         visited-nodes #{}
         paths '()]
    (if (empty? nodes) paths
        (let [[[pos path-counter constraints :as node] & r] nodes]
          (if (= pos target)
            (recur r
                   visited-nodes
                   (cons [path-counter constraints] paths))
            (let [neighbors (find-neighbors m visited-nodes pos)
                  new-path-counter (inc path-counter)
                  neighbors-nodes (map (fn [[p c]]
                                         [p new-path-counter (reduce add-key constraints c)]) neighbors)]
              (recur (doall (concat r neighbors-nodes))
                     (apply conj (conj visited-nodes pos) (map first neighbors))
                     paths)))))))

(defn map-paths-per-constraint [paths]
  (into {} (map (fn [[p c]] [c p])) paths))

(defn preprocess-map [m start-pos key-positions]
  (let [key-combos (combo/combinations (cons start-pos key-positions) 2)
        paths (->> key-combos
                   (map #(vector % (map-paths-per-constraint (find-paths m (first %) (second %)))))
                   (mapcat (fn [[[p1 p2] result]]
                             (let [k1 (m p1)
                                   k2 (m p2)]
                               [[k1 [k2 result]] [k2 [k1 result]]])))
                   (group-by first)
                   (map (fn [[k v]] (vector k (into {} (map second v)))))
                   (into {}))]
    paths))

(defn has-keys [path-mask mask]
  (= (bit-and path-mask mask) path-mask))

(defn find-shortest-path [m paths start-pos number-of-keys]
  (let [all-keys-mask (reduce bit-set 0 (range 0 number-of-keys))
        start-mask 0]
    (loop [nodes (pm/priority-map [start-pos start-mask] 0)
           solutions '()
           global-visited-nodes #{}]
      (if (empty? nodes) solutions
          (let [[[pos mask :as n] counter] (peek nodes)
                r (pop nodes)
                visited-nodes (conj global-visited-nodes n)]
            (cond
              (contains? global-visited-nodes n) (recur r
                                                        solutions
                                                        global-visited-nodes)
              (= mask all-keys-mask) [pos counter]

              :else (let [neighbor-nodes (->> pos
                                              paths
                                              (filter (fn [[n-pos]]
                                                        (and
                                                         (not= n-pos start-pos)
                                                         (not (has-key mask n-pos)))))
                                              (keep (fn [[n-pos p]]
                                                      (let [distance (->> p
                                                                          (filter #(has-keys (first %) mask))
                                                                          (map second)
                                                                          sort
                                                                          first)]
                                                        (let [new-mask (add-key mask n-pos)
                                                              new-n [n-pos new-mask]]
                                                          (if (and (not (nil? distance))
                                                                   (not (contains? visited-nodes new-n)))
                                                            (let [updated-counter (+ counter distance)]
                                                              [new-n updated-counter]))))))
                                              (into {}))]

                      (recur (merge-with min r neighbor-nodes)
                             solutions
                             visited-nodes))))))))

(defn part-1 []
  (let [input (load-input)
        m (parse-map input)
        [start-pos] (find-start-positions m)
        key-pos (find-keys m)
        paths (preprocess-map m start-pos key-pos)]
    (find-shortest-path m paths \@ (count key-pos))))

(defn load-input-2 []
  (slurp "inputs/day18/input2"))

(defn digit? [c]
  (Character/isDigit ^char c))

(defn preprocess-map-2 [m start-positions key-positions]
  (let [key-combos (combo/combinations (concat start-positions key-positions) 2)
        paths (->> key-combos
                   (filter (fn [[p1 p2]]
                             (not (and (digit? (m p1))
                                       (digit? (m p2))))))
                   (map #(vector % (map-paths-per-constraint (find-paths m (first %) (second %)))))
                   (filter #(not (empty? (second %))))
                   (mapcat (fn [[[p1 p2] result]]
                             (let [k1 (m p1)
                                   k2 (m p2)]
                               (cond
                                 (not (or (digit? k1)
                                          (digit? k2))) [[k1 [k2 result]] [k2 [k1 result]]]

                                 (digit? k1) [[k1 [k2 result]]]
                                 (digit? k2) [[k2 [k1 result]]]))))
                   (group-by first)
                   (map (fn [[k v]] (vector k (into {} (map second v)))))
                   (into {}))]
    paths))

(defn improve-map [m start-positions]
  (let [number-start-positions (into {} (map-indexed #(vector %2 (char (+ (int \0) %))) start-positions))
        updated-map (merge m number-start-positions)]
    updated-map))

(defn find-neighbors-2 [positions counter paths mask visited-nodes]
  (->> (range 0 (count positions))
       (mapcat (fn [n] (let [pos (nth positions n)]
                         (->> pos
                              paths
                              (filter (fn [[n-pos]]
                                        (and
                                         (not (Character/isDigit ^char n-pos))
                                         (not (has-key mask n-pos)))))
                              (keep (fn [[n-pos p]]
                                      (let [distance (->> p
                                                          (filter #(has-keys (first %) mask))
                                                          (map second)
                                                          sort
                                                          first)]
                                        (let [new-mask (add-key mask n-pos)
                                              new-n [n-pos new-mask]]
                                          (if (and (not (nil? distance))
                                                   (not (contains? visited-nodes new-n)))
                                            (let [updated-counter (+ counter distance)]
                                              [[(assoc positions n n-pos) new-mask] updated-counter]))))))))))
       (into {})))

(defn find-shortest-path-2 [m paths start-positions key-positions]
  (let [all-keys-mask (reduce add-key 0 (map m key-positions))
        start-mask 0]
    (loop [nodes (pm/priority-map [(vec start-positions) start-mask] 0)
           solutions '()
           global-visited-nodes #{}]
      (if (empty? nodes) solutions
          (let [[[positions mask :as n] counter] (peek nodes)
                r (pop nodes)
                visited-nodes (conj global-visited-nodes n)]
            (cond
              (contains? global-visited-nodes n) (recur r
                                                        solutions
                                                        global-visited-nodes)
              (= mask all-keys-mask) [positions counter]
              :else (let [neighbor-nodes (find-neighbors-2 positions counter paths mask visited-nodes)]
                      (recur (merge-with min r neighbor-nodes)
                             solutions
                             visited-nodes))))))))

(defn part-2 []
  (let [input (load-input-2)
        m (parse-map input)
        start-positions (find-start-positions m)
        key-positions (find-keys m)
        updated-map (improve-map m start-positions)
        start-nodes (map updated-map start-positions)
        paths (preprocess-map-2 updated-map start-positions key-positions)]
    (find-shortest-path-2 m paths start-nodes key-positions)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
