(ns aoc-2021.day10
  (:require [clojure.string :as str]))

(def input
  (->>
    (str/split (slurp "inputs/aoc_2021/input_day10.txt") #"\n")))

(defn syntax-score [row]
  ({\) 3 \] 57 \} 1197 \> 25137 nil 0} (first row)))

(defn closing-char? [c]
  (contains? #{\) \> \] \}} c))

(defn remove-pairs [row]
  (loop [remaining row]
    (let [filtered (str/replace remaining #"\(\)|\[\]|\{\}|\<\>" "")]
      (if (= remaining filtered)
        filtered
        (recur filtered)))))

(syntax-score (filter closing-char? (remove-pairs (first input))))
(defn part1 []
  (->> input
       (map remove-pairs)
       (map #(filter closing-char? %))
       (map syntax-score)
       (reduce +)))

(defn autocomplete-score [row]
  (->> row
       (map {\( 1 \[ 2 \{ 3 \< 4 })
       reverse
       (reduce #(+ (* 5 %1) %2) 0)))

(defn part2 []
  (let [scores (->> input
                    (map remove-pairs)
                    (filter #(not-any? closing-char? %))
                    (map autocomplete-score)
                    sort)]
  (nth scores (/ (count scores) 2))))

(comment
  (part1)
  (part2))
