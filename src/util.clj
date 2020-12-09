(ns util
  (:require [clojure.string :as string]))

(defn load-lines [day]
  (string/split-lines (slurp (str "inputs/day" day))))
