(ns day4)

(defn get-digits [number]
  (for [n (->> number
               (iterate #(int (/ % 10)))
               (take-while #(> % 0)))]
    (rem n 10)))

(defn contains-double-digits? [n]
  (->> n
       get-digits
       (partition-by identity)
       (map count)
       (some #(> % 1))))

(defn digits-never-decrease? [n]
  (loop [digits (reverse (get-digits n))
         last-digit nil]
    (if (seq digits)
      (let [d (first digits)]
        (if (and (not (nil? last-digit)) (> last-digit d))
          false
          (recur (rest digits) d)))
      true)))

(defn part-1 []
  (let [from 158126
        to 624574]
    (->>
     (range from to)
     (filter contains-double-digits?)
     (filter digits-never-decrease?)
     count)))

(defn contains-doubles-not-part-of-larger-group? [n]
  (->> (get-digits n)
       (partition-by identity)
       (map count)
       (some #(= 2 %))))

(defn part-2 []
  (let [from 158126
        to 624574]
    (->>
     (range from to)
     (filter contains-doubles-not-part-of-larger-group?)
     (filter digits-never-decrease?)
     count)))

(defn -main [& args]
  (println (part-1))
  (println (part-2)))

