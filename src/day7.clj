(ns day7
  (:require [clojure.string :as string]
            [clojure.pprint]
            [clojure.java.io]))

(comment
  (def i (string/split-lines (slurp "inputs/day7"))))

(def line-pattern #"(?:(\d+|\w+) )?(\w+ \w+) bags?")

(defn parse-line [line]
  (let [[[_ _ bag] & r] (re-seq line-pattern line)]
    (if (and (= (count r) 1)
             (string/includes? line "no other bags"))
      {:color bag}
      {:color bag
       :contains (into {} (map (fn [[_ n c]] (vector c (Integer/parseInt n))) r))})))

(comment
  (def test-str "plaid magenta bags contain 2 clear lavender bags, 3 clear teal bags, 4 vibrant gold bags.")

  (parse-line test-str)
  (parse-line "shiny gold bags contain 2 dim beige bags, 1 dark maroon bag, 4 light blue bags.")
  (re-seq line-pattern "shiny gold bags contain 2 dim beige bags, 1 dark maroon bag, 4 light blue bags.")
  (keys (:contains (parse-line test-str)))

  (map parse-line i)

  (re-matches #".*" test-str)
  (re-matches #"\d*" test-str)
  (re-seq #"(?:(\d+|\w+) )?(\w+ \w+) bags" test-str)
  (re-seq #"(?:((\d+|(?!no)\w{2,})) )?(\w+ \w+) bags" "drab coral bags contain no other bags.")

  (let [[a & b] [1 2 3 4]]
    (vector a b)))

(defn reversed-dependencies [m]
  (into {}
        (map (fn [[k v]]
               (vector k (into #{} (map second v))))
             (group-by first
                       (mapcat (fn [[k v]]
                                 (map #(vector % k) v)) m)))))

(defn to-dependency-map [bags]
  (into {}
        (map (fn [{:keys [color contains]}]
               (vector color (keys contains))) bags)))

(defn find-bags-containing-shiny-gold [bags]
  (let [start "shiny gold"
        dependencies (to-dependency-map bags)
        reversed-dependencies (reversed-dependencies dependencies)]
    (loop [colors #{}
           [current & r :as q] (reversed-dependencies start)]
      (cond
        (nil? current) colors
        (contains? colors current) (recur colors r)
        :else (let [dependencies (reversed-dependencies current)]
                (recur (conj colors current)
                       (concat r dependencies)))))))

(comment
  (let [parsed (map parse-line i)]
    (count (find-bags-containing-shiny-gold parsed))))

(defn to-dependency-map-with-counts [bags]
  (into {}
        (map (fn [{:keys [color contains]}]
               (vector color contains)) bags)))

(defn count-needed-bags [bags]
  (let [dependencies-with-counts (to-dependency-map-with-counts bags)]
    (loop [cnt 0
           [[color bag-cnt :as cur] & r] (list '("shiny gold" 1))]
      (cond
        (nil? cur) (dec cnt)
        :else (let [deps (dependencies-with-counts color)]
                (recur (+ cnt bag-cnt)
                       (concat r (map (fn [[k v]] (vector k (* v bag-cnt))) deps))))))))

(comment
  (let [parsed (map parse-line i)]
    (count-needed-bags parsed)))

(defn -main [& args]
  (let [lines (string/split-lines (slurp "inputs/day7"))
        parsed (map parse-line lines)]
    (println (count (find-bags-containing-shiny-gold parsed)))
    (println count-needed-bags parsed)))
