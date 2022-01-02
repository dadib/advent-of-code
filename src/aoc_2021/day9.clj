(ns aoc-2021.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (->>
    (str/split (slurp "inputs/aoc_2021/input_day9.txt") #"\n")
    (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #""))))))

(defn valid-neighbors [row col]
  (->>
    [[(inc row) col] [(dec row) col] [row (inc col)] [row (dec col)]]
    (filter (fn [[r c]] (and (>= r 0)
                             (>= c 0)
                             (< r (count input))
                             (< c (count (first input))))))))

(defn height [row col] (nth (nth input row) col))

(defn low-points []
  (for [row (range (count input))
        col (range (count (first input)))
        :let [v (height row col)]
        :when (every? #(< v %) (mapv #(apply height %) (valid-neighbors row col)))]
    [row col]))

(defn part1 []
  (reduce + (map #(inc (apply height %)) (low-points))))

(defn basin-points
  "expand from point to form basin"
  [row column]
  (loop [point-set #{[row column]}]
    (let [expanded-point-set (->> point-set
                                  (mapcat #(apply valid-neighbors %))
                                  (filter #(not= 9 (apply height %)))
                                  set
                                  (set/union point-set))]
      (if (= point-set expanded-point-set)
        expanded-point-set
        (recur expanded-point-set)))))

(defn part2 []
  (->> (low-points)
       (map #(apply basin-points %))
       (sort-by #(- (count %)))
       (take 3)
       (map count)
       (reduce *)))

(comment
  (part1)
  (part2))
