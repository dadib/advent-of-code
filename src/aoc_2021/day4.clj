(ns aoc-2021.day4
  (:require [clojure.string :as str]))

(defn parse-board [board]
  (-> board
      (str/replace #"[\n|\s]+" " ")
      (str/trim)
      (str/split #"\s")
      (->> (mapv #(Integer/parseInt %)))))

(def input
  (let [[draws boards] (str/split (slurp "inputs/aoc_2021/input_day4.txt") #"\n" 2)]
    {:draws  (mapv #(Integer/parseInt %) (str/split draws #","))
     :boards (mapv parse-board (str/split boards #"\n\n"))}))

(defn bingo? [board numbers]
  (let [rows (partition 5 board)
        columns (map #(flatten (partition 1 5 (drop % board))) (range 5))
        num-set (set numbers)]
    (true? (some #(every? num-set %) (concat rows columns)))))

(defn score [board numbers]
  (let [unmarked (remove (set numbers) board)]
    (* (reduce + unmarked) (last numbers))))

(defn winner-seq []
  "score for each winning board in order"
  (->>
    (range 1 (inc (count (:draws input))))
    (map #(take % (:draws input)))
    (reductions
      (fn [[_ remaining-boards] numbers]
        (let [{winners true
               not-winners false} (group-by #(bingo? % numbers) remaining-boards)]
          [(map #(score % numbers) winners) not-winners]))
      [[] (:boards input)])
    (mapcat #(first %))))

(defn part1 [] (first (winner-seq)))
(defn part2 [] (last (winner-seq)))

(comment
  (part1)
  (part2))