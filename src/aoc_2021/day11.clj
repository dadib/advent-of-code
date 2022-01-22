(ns aoc-2021.day11
  (:require [clojure.string :as str]))

(defn input []
  (let [text (slurp "inputs/aoc_2021/input_day11.txt")]
    [(->> text
          (filter #(not= \newline %))
          (map #(Integer/parseInt (str %)))
          int-array)
     (inc (count (filter #(= \newline %) text)))]))

(defn nrow [pos arr nrows]
  (if (and (< pos (count arr)) (>= pos 0))
    (int (/ pos (/ (count arr) nrows)))
    nil))

(defn neighbor-indices [pos arr nrows]
  (let [row (nrow pos arr nrows)
        ncolumns (/ (count arr) nrows)]
    (concat
      (filter #(= row (nrow % arr nrows)) [(inc pos) (dec pos)])
      (filter #(= (inc row) (nrow % arr nrows)) (take 3 (iterate inc (+ pos (dec ncolumns)))))
      (filter #(= (dec row) (nrow % arr nrows)) (take 3 (iterate inc (- pos (inc ncolumns))))))))

(defn ainc [arr pos]
  (aset arr pos (inc (aget arr pos))))

(defn step! [arr nrows]
  (dotimes [i (count arr)] (ainc arr i))                    ;; inc each
  (loop []
    (let [flash-indices (filter #(> (aget arr %) 9) (range (count arr)))]
      (doseq [i flash-indices]
        (aset arr i 0)
        (doseq [neighbor (neighbor-indices i arr nrows)]
          (if (not= 0 (aget arr neighbor)) (ainc arr neighbor)))) ;; inc flash neighbors
      (if (not-empty flash-indices) (recur))))
  (count (filter #(= 0 %) arr)))

(defn part1 []
  (let [[arr nrows] (input)]
    (reduce + (repeatedly 100 #(step! arr nrows)))))

(defn part2 []
  (let [[arr nrows] (input)]
    (->> (repeatedly #(step! arr nrows))
         (take-while (fn [_] (not (every? zero? arr))))
         count
         inc)))

(comment
  (part1)
  (part2))