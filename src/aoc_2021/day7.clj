(ns aoc-2021.day7
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def input
  (->>
    (str/split (slurp "inputs/aoc_2021/input_day7.txt") #",")
    (mapv #(Integer/parseInt %))))

(defn median [items]
  (first (drop (int (/ (count items) 2)) (sort items))))

(defn l1 [pos values]
  (reduce + (map #(math/abs (- % pos)) values)))

(defn part1
  "The median minimizes the sum of absolute deviations"
  []
  (l1 (median input) input))

(defn part2 []
  (let [minpos (apply min input)
        maxpos (apply max input)
        costfunc (fn [pos]
                   ;; The fuel cost for a given crab is cost = \\sum_{i}^(pos-x_c) i = |pos - x| * (|pos - x| + 1) / 2
                   (reduce + (map #(/ (* (math/abs (- % pos)) (+ 1 (math/abs (- % pos)))) 2) input))) ]
    (->> (range minpos (inc maxpos))
         (mapv costfunc)
         (reduce min))))

(comment
  (part1)
  (part2))