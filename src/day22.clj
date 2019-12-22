(ns day22
  (:require
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn load-input []
  (slurp "inputs/day22/input1"))

(defn parse-line [line]
  (cond
    (string/starts-with? line "deal into new stack") [:deal-new-stack]
    (string/starts-with? line "cut") (let [[_ amount] (re-matches #"cut (-?\d*)" line)]
                                       [:cut (Long/parseLong ^String amount)])
    (string/starts-with? line "deal with increment") (let [[_ amount] (re-matches #"deal with increment (\d*)" line)]
                                                       [:deal-with-increment (Long/parseLong amount)])))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map parse-line)
       doall))

(defmulti instruction->coefficiencts (fn [_ [t]] t))

(defmethod instruction->coefficiencts :deal-new-stack [number-of-cards _]
  [-1 (dec number-of-cards)])

(defmethod instruction->coefficiencts :cut [number-of-cards [_ amount]]
  [1 (- (biginteger amount))])

(defmethod instruction->coefficiencts :deal-with-increment [number-of-cards [_ amount]]
  [(biginteger amount), 0])

(defn compose [size [a b] [c d]]
  [(mod (* a c) size)
   (mod (+ (* a d) b) size)])

(defn execute [x a b]
  (+ (* a x) b))

(defn part-1 []
  (let [input (load-input)
        parsed-input (parse-input input)
        number-of-cards 10007
        searched 2019]
    (->> parsed-input
         (map (partial instruction->coefficiencts number-of-cards))
         reverse
         (reduce (partial compose number-of-cards) [1 0])
         (#(mod (apply execute searched %) number-of-cards)))))

(defn exp-by-square [a b n size]
  (if (= n 1) [a b]
      (let [[new-a new-b :as r1] (compose size [a b] [a b])]
        (if (even? n)
          (recur new-a new-b (/ n 2) size)
          (let [r2 (exp-by-square new-a new-b (/ (dec n) 2) size)]
            (compose size [a b] r2))))))

(defn combine [number-of-cards parsed-input]
  (->> parsed-input
       (map (partial instruction->coefficiencts number-of-cards))
       reverse
       (reduce (partial compose number-of-cards) [1 0])))

(defn part-2 []
  (let [input (load-input)
        parsed-input (parse-input input)
        number-of-cards (biginteger 119315717514047)
        rep (biginteger 101741582076661)
        searched-pos 2020
        [a b] (combine number-of-cards parsed-input)
        [a1 b1] (exp-by-square a b rep number-of-cards)
        inv-a (.modInverse (biginteger a1) number-of-cards)
        res (mod (- (mod (* inv-a searched-pos) number-of-cards)
                    (mod (* inv-a b1) number-of-cards)) number-of-cards)]
    res))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))
