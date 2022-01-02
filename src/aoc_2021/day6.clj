(ns aoc-2021.day6
  (:require [clojure.string :as str]))

(def input
  (->>
    (str/split (slurp "inputs/aoc_2021/input_day6.txt") #",")
    (mapv #(Integer/parseInt %))
    frequencies))

(defn fish-step [state]
  (reduce-kv (fn [new-state days counts]
               (if (zero? days)
                 (merge-with + new-state {6 counts 8 counts})
                 (merge-with + new-state {(dec days) counts})))
             {} state))

(defn n-fish [steps]
  (->>
    (iterate fish-step input)
    (take (inc steps))
    last
    vals
    (reduce +)))

(comment
  (n-fish 80)
  (n-fish 256))