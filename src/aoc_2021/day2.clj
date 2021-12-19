(ns aoc-2021.day2
  (:require [clojure.string :as str]))

(def input
  (->>
    (slurp "inputs/aoc_2021/input_day2.txt")
    (str/split-lines)
    (mapv #(let [[cmd value] (str/split % #"\s")]
             [(keyword cmd) (Integer/parseInt value)]))))

(defn part1 []
  (let [[x depth] (reduce (fn [[x depth] [cmd value]]
                           (case cmd
                             :forward [(+ x value) depth]
                             :up [x (- depth value)]
                             :down [x (+ depth value)]))
                         [0 0] input)]
    (* x depth)))

(defn part2 []
  (let [[x depth] (reduce (fn [[x depth aim] [cmd value]]
                            (case cmd
                              :forward [(+ x value) (+ depth (* value aim)) aim]
                              :up [x depth (- aim value)]
                              :down [x depth (+ aim value)]))
                          [0 0 0] input)]
    (* x depth)))

(comment
 (part1)
 (part2))