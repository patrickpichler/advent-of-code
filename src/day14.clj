(ns day14
  (:require [clojure.string :as string]
            [clojure.algo.generic.math-functions :refer [ceil]]))

(defn load-input []
  (slurp "inputs/day14/input1"))

(defn parse-input-string [input]
  (let [[_ n i] (re-matches #"(\d*) ([A-Za-z]*)" input)]
    [i (Long/parseLong n)]))

(defn parse-inputs-string [inputs]
  (->> inputs
       (#(string/split % #","))
       (map string/trim)
       (map parse-input-string)
       (into {})))

(defn parse-line [line]
  (->> line
       (#(string/split % #"=>"))
       (#(vector (parse-input-string (string/trim (second %))) (parse-inputs-string (first %))))))

(defn parse-input []
  (->> (load-input)
       string/split-lines
       (map parse-line)
       (map (fn [[[i n] r]] [i [n r]]))
       (into {})))

(defn consume-chemicals [leftovers chemical amount]
  (update leftovers chemical (fnil - 0) amount))

(defn add-chemicals [required chemical amount]
  (update required chemical (fnil + 0) amount))

(defn reduce-chemicals-required [[leftovers to-produce] [chemical amount]]
  (let [leftover-amount (leftovers chemical 0)]
    (if (>= leftover-amount amount)
      [(consume-chemicals leftovers chemical amount) to-produce]
      (let [amount-left-to-produce (- amount leftover-amount)]
        [(consume-chemicals leftovers chemical leftover-amount)
         (cons [chemical amount-left-to-produce] to-produce)]))))

(defn merge-produce-queue [queue]
  (->> queue
       (reduce (fn [[q m] [chemical amount]]
                 (if (contains? m chemical)
                   [q (add-chemicals m chemical amount)]
                   [(cons chemical q) (add-chemicals m chemical amount)])) ['() {}])
       ((fn [[q m]]
          (->> q
               reverse
               (map #(vector % (m %))))))))

(defn calculate-ore-required
  ([input to-produce]
   (loop [to-produce-queue (list to-produce)
          leftovers {}
          required {}]
     (if (empty? to-produce-queue) required
         (let [merged-production-queue (merge-produce-queue to-produce-queue)
               [[chemical amount]] merged-production-queue
               [amount-per-produce chemicals-required] (input chemical [1 {}])
               number-of-prods (long (ceil (/ amount amount-per-produce)))
               total-chemicals-required (map #(vector (first %) (* (second %) number-of-prods)) chemicals-required)
               [updated-leftovers chemicals-to-produce] (reduce reduce-chemicals-required
                                                                [leftovers (rest merged-production-queue)]
                                                                total-chemicals-required)
               updated-required (add-chemicals required chemical amount)]
           (recur chemicals-to-produce
                  (add-chemicals updated-leftovers chemical (- (* amount-per-produce number-of-prods) amount))
                  updated-required))))))

(defn part-1 []
  (let [input (parse-input)
        {ore "ORE"} (calculate-ore-required input ["FUEL" 1])]
    ore))

(def max-ore 1000000000000)

(defn part-2 []
  (let [input (parse-input)]
    (loop [number-of-fuel 1
           last-number-of-fuel 0
           step-size 1000000]
      (let [{needed-ore "ORE"} (calculate-ore-required input ["FUEL" number-of-fuel])]
        (cond
          (and (= step-size 1) (> needed-ore max-ore)) last-number-of-fuel

          (> needed-ore max-ore) (recur last-number-of-fuel
                                        last-number-of-fuel
                                        (/ step-size 10))

          (< needed-ore max-ore) (recur (+ number-of-fuel step-size)
                                        number-of-fuel
                                        step-size)
          :else number-of-fuel)))))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))

