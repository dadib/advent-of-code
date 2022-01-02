(ns aoc-2021.day5
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn parse-points [line]
  (let [points (str/split line #" -> ")]
    (mapv
      (fn [points] (mapv #(Integer/parseInt %) (str/split points #",")))
      points)))

(def input
  (->
    (slurp "inputs/aoc_2021/input_day5.txt")
    (str/split #"\n")
    (->> (map parse-points))))

(defn sign [n]
  (/ n (math/abs n)))

(defn intermediate-points [p1 p2]
  (let [[x1 y1] p1 [x2 y2] p2]
    (cond
      (= x1 x2) (let [diff (math/abs (- y2 y1))
                      sign (sign (- y2 y1))]
                  (map #(vector x1 (+ y1 (* sign %))) (range 0 (inc diff))))
      (= y1 y2) (let [diff (math/abs (- x2 x1))
                      sign (sign (- x2 x1))]
                  (map #(vector (+ x1 (* sign %)) y1) (range 0 (inc diff))))
      :else (let [diff (math/abs (- x2 x1))
                  xsign (sign (- x2 x1))
                  ysign (sign (- y2 y1))]
              (map #(vector (+ x1 (* xsign %)) (+ y1 (* ysign %))) (range 0 (inc diff))))))
  )

(defn point-counts [points]
  (reduce (fn [counter point] (update-in counter point #(if (= % nil) 1 (inc %)))) {} points))

(defn larger-than-one [point-counts]
  (count (filter #(> % 1) (mapcat vals (vals point-counts)))))

(defn part1 []
  (->> input
       (filter #(let [[[x1 y1] [x2 y2]] %] (or (= x1 x2) (= y1 y2))))
       (mapcat #(apply intermediate-points %))
       (point-counts)
       (larger-than-one)))

(defn part2 []
  (->> input
       (mapcat #(apply intermediate-points %))
       (point-counts)
       (larger-than-one)))

(comment
  (part1)
  (part2))
