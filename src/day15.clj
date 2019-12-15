(ns day15
  (:require
   [clojure.string :as string]
   [util.intcode :as intcode]))

(defn load-input []
  (slurp "inputs/day15/input1"))

(defn create-ai-state
  ([state m current-pos action-path action paths-to-target]
   (intcode/create-computer-run-state state [m current-pos action-path action paths-to-target] (if (nil? action) [] [action]))))

(defn create-shortest-path-found-state [m shortest-path-count]
  (intcode/create-computer-run-state :TERMIATED [m shortest-path-count] []))

(defn translate-pos [[x y] action]
  (case action
    1 [x (dec y)]
    2 [x (inc y)]
    3 [(dec x) y]
    4 [(inc x) y]
    [x y]))

(defn response->tile [r]
  (case r
    0 :WALL
    1 :FREE
    2 :OXYGEN_SYSTEM))

(defn update-map [m pos response]
  (assoc m pos (response->tile response)))

(defn find-next-way-to-go [m pos]
  (cond
    (not (contains? m (translate-pos pos 1))) 1
    (not (contains? m (translate-pos pos 2))) 2
    (not (contains? m (translate-pos pos 3))) 3
    (not (contains? m (translate-pos pos 4))) 4
    :else nil))

(defn find-next-action [m pos response [last-action]]
  (find-next-way-to-go m pos))

(defn find-shortest-path-length [m]
  0)

(defn reverse-action [action]
  (case action
    1 2
    2 1
    3 4
    4 3))

(defn run-ai [m current-pos action-path last-action paths-to-target input]
  (if (empty? input)
    (create-ai-state :WAITING_FOR_INPUT m current-pos '() 1 '())
    (let [[result] input
          translated-pos (translate-pos current-pos last-action)
          updated-map (update-map m translated-pos result)
          pos-after-action (if (= 0 result) current-pos translated-pos)
          next-action (find-next-action updated-map pos-after-action result action-path)
          corrected-action-path (if (not= result 0) action-path (rest action-path))]
      (cond
        (and (= (count action-path) 1) (nil? next-action)) (create-shortest-path-found-state updated-map (reduce max (map count paths-to-target)))
        (nil? next-action) (let [reversed-action (reverse-action (first corrected-action-path))]
                             (create-ai-state :WAITING_FOR_INPUT
                                              updated-map
                                              pos-after-action
                                              (rest corrected-action-path)
                                              reversed-action
                                              paths-to-target))
        :else (create-ai-state :WAITING_FOR_INPUT
                               updated-map
                               pos-after-action
                               (cons next-action corrected-action-path)
                               next-action
                               (if (= result 2)
                                 (cons action-path paths-to-target)
                                 paths-to-target))))))

(defn run-ai-with-args [[m current-pos action-path action paths-to-target] input args]
  (run-ai m current-pos (or action-path '()) action (or paths-to-target '()) (concat args input)))

(defn create-robot-state
  ([current-pos m result]
   (create-robot-state :WAITING_FOR_INPUT current-pos m result))

  ([state current-pos m result]
   (intcode/create-computer-run-state state [current-pos m] (if (nil? result) [] [result]))))

(defn run-robot
  ([data input args] (run-robot data (concat args input)))

  ([[current-pos m] input]
   (if (empty? input)
     (create-robot-state current-pos m nil)
     (let [[action] input
           updated-pos (translate-pos current-pos action)
           result (m updated-pos 1)]
       (create-robot-state (if (not= 0 result) updated-pos current-pos)
                           m
                           result)))))

(defn parse-line [rownum line]
  (->> line
       (map #(case %
               \W 0
               \X 2
               1))
       (map-indexed #(vector [%1 rownum] %2))))

(defn part-1 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        vms (intcode/run {:1 (intcode/create-intcode-vm memory [])
                          :ai (intcode/create-vm-state run-ai-with-args [{[1 1] 1} [1 1] '()]) }

                         {:1 [:ai]
                          :ai [:1] })
        [_ _ [_ distance]] (:ai vms)]
    distance))

(defn get-neighbors [m pos]
  (->> (range 1 5)
       (map #(translate-pos pos %))
       (filter #(not= (m %) :WALL))
       (into [])))

(defn fill-with-oxygen [m start-pos]
  (loop [distance-map {start-pos 0}
         visited-nodes #{}
         to-visit (list start-pos)]
    (if (empty? to-visit) (reduce max (map second distance-map))
        (let [current-node (first (sort-by #(distance-map % Integer/MAX_VALUE) to-visit))
              nodes (remove (partial = current-node) to-visit)]
          (let [neighbors (get-neighbors m current-node)
                distance (distance-map current-node)
                neighbors-distance (inc distance)
                new-neighbors (filter #(not (contains? visited-nodes %)) neighbors)
                updated-distance-map (->> neighbors
                                          (reduce (fn [m p] (update m p (fnil min Integer/MAX_VALUE) neighbors-distance)) distance-map))]
            (recur updated-distance-map
                   (conj visited-nodes current-node)
                   (concat new-neighbors nodes)))))))

(defn part-2 []
  (let [input (load-input)
        memory (intcode/parse-intcode-memory input)
        vms (intcode/run { :1 (intcode/create-intcode-vm memory [])
                          :ai (intcode/create-vm-state run-ai-with-args [{[1 1] 1} [1 1]])
                          }

                         { :1 [:ai]
                          :ai [:1]
                          })
        [_ _ [m]] (:ai vms)
        target-pos (first (map first (filter (fn [[pos t]] (= t :OXYGEN_SYSTEM)) m)))]
    (fill-with-oxygen m target-pos)))

(defn -main []
  (println (part-1))
  (println (part-2)))
