(ns aoc-2021.day1
  (:require [clojure.string :as str]))

(def input
  (->>
    (slurp "inputs/aoc_2021/input_day1.txt")
    (str/split-lines)
    (mapv #(Integer/parseInt %))))

(defn part1 []
  (count
    (filter pos? (map #(- (last %) (first %)) (partition 2 1 input)))))

(defn part2 []
  (count
    (filter pos? (map #(- (last %) (first %)) (partition 4 1 input)))))

(comment
  (part1)
  (part2))

