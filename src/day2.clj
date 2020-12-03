(ns day2
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (let [[_ min max char password] (re-matches #"(\d*)-(\d*) ([a-z]): ([a-z]*)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :char (first char)
     :password password}))

(defn is-valid-part1? [{:keys [min max char password]}]
  (let [c (count (filter #(= % char) password))]
    (<= min c max)))

(comment
  (is-valid-part1? {:min 1 :max 2 :char \c :password "password"})
  (is-valid-part1? {:min 1 :max 2 :char \a :password "password"})
  (is-valid-part1? {:min 4 :max 8 :char \g :password "ggtxgtgbg"})
  (is-valid-part1? {:min 1 :max 3 :char \b :password "cdefg"})
  (is-valid-part1? {:min 2 :max 9 :char \c :password "ccccccccc"})
  (is-valid-part1? {:min 13 :max 14 :char \q :password "qmzdrtqctvrqsb"})
  (count (filter #(do (println %) (= % \a)) "password"))
  (def i (map parse-line
              (string/split-lines (slurp "inputs/day2"))))
  (count (filter is-valid-part1? i))
  (first (filter is-valid-part1? i))

  (<= 1 3 3))

(defn is-valid-part2? [{f :min s :max :keys [char password]}]
  (let [first-match? (= (nth password (dec f)) char)
        second-match? (= (nth password (dec s)) char)]
    (and (or first-match? second-match?)
         (not= first-match? second-match?))))

(comment
  (is-valid-part2? {:min 1 :max 2 :char \c :password "password"})
  (is-valid-part2? {:min 1 :max 2 :char \p :password "password"})
  (is-valid-part2? {:min 1 :max 3 :char \s :password "password"})
  (is-valid-part2? {:min 1 :max 3 :char \p :password "pap"})
  (is-valid-part2? {:min 1 :max 3 :char \p :password "par"})

  (def i (map parse-line
              (string/split-lines (slurp "inputs/day2"))))
  (count (filter is-valid-part2? i))

  (nth "hello" 1))

(defn -main [& args]
  (let [input (slurp "inputs/day2")
        parsed-input (map parse-line (string/split-lines input))]
    (println (count (filter is-valid-part1? parsed-input)))
    (println (count (filter is-valid-part2? parsed-input)))))
