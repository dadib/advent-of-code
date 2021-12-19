(ns aoc-2021.day3
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def input
  (->>
    (slurp "inputs/aoc_2021/input_day3.txt")
    (str/split-lines)
    (map (fn [row] (mapv #(Integer/parseInt (str %)) row)))))

(defn binary-to-int [b]
  (reduce +
          (map-indexed
            (fn [idx bit] (if (= 1 bit) (math/expt 2 idx) 0))
            (reverse b))))

(defn most-common-bit? [column value rows]
  "1 if most occurrences in column idx are value, else 0"
  (if (->>
     rows
     (map #(get % column))
     (filter #(= value %))
     (count)
     (<= (/ (count rows) 2)))
    1 0))

(defn part1 []
  (let [n_columns (count (first input))
        epsilon (binary-to-int (map #(most-common-bit? % 1 input) (range n_columns)))
        gamma (binary-to-int (map #(most-common-bit? % 0 input) (range n_columns)))]
    (* epsilon gamma)))

(defn life-support-rating [metric]
  (loop [[rows column] [input 0]]
    (let [filter-bit (case metric
                       :oxygen (most-common-bit? column 1 rows)
                       :co2 (mod (+ 1 (most-common-bit? column 1 rows)) 2))
          remaining (filter #(= filter-bit (get % column)) rows)]
      (if (= 1 (count remaining))
        (binary-to-int (first remaining))
        (recur [remaining (+ 1 column)])))))

(defn part2 []
  (let [oxygen (life-support-rating :oxygen)
        co2 (life-support-rating :co2)]
    (* oxygen co2)))

(comment
  (part1)
  (part2))