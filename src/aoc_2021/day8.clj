(ns aoc-2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def digits
  {#{\a \b \c \e \f \g} \0
   #{\c \f} \1
   #{\a \c \d \e \g} \2
   #{\a \c \d \f \g} \3
   #{\b \c \d \f} \4
   #{\a \b \d \f \g} \5
   #{\a \b \d \e \f \g} \6
   #{\a \c \f} \7
   #{\a \b \c \d \e \f \g} \8
   #{\a \b \c \d \f \g} \9})

(defn parse-input [filename]
  (->>
    (str/split (slurp filename) #"\n")
    (map (fn [line]
           (let [[inputs outputs] (str/split line #" \| ")]
             [(mapv set (str/split inputs #" ")) (mapv set (str/split outputs #" "))])))))

(def input (parse-input "inputs/aoc_2021/input_day8.txt"))

(defn part1 []
  (->>
    (map second input)
    flatten
    (filter #(contains? #{2 3 4 7} (count %)))
    count))

(defn decode-row [row]
  (let [[inputs outputs] row
        mapping (let [counts (frequencies (flatten (map seq inputs)))
                      one (first (filter #(= 2 (count %)) inputs))
                      seven (first (filter #(= 3 (count %)) inputs))
                      four (first (filter #(= 4 (count %)) inputs))
                      eight (first (filter #(= 7 (count %)) inputs))
                      nine (first (filter #(and
                                             (empty? (set/difference (set/union seven four) %))
                                             (= 1 (count (set/difference % (set/union seven four))))) inputs))
                      c-and-f (set/intersection seven one)
                      b-and-d (set/difference four seven)]
                  (-> {}
                      (assoc (first (set/difference seven one)) \a) ;; a is the field unique to seven
                      (assoc (first (set/difference nine (set/union seven four))) \g) ;; g is in nine but not seven and four
                      (assoc (first (set/difference eight nine)) \e) ;; e is in eight but not nine
                      (into (map
                              (fn [entry] {(key entry)
                                           (case (val entry)
                                             8 \c
                                             9 \f)})
                              (select-keys counts c-and-f)))
                      (into (map
                              (fn [entry] {(key entry)
                                           (case (val entry)
                                             6 \b
                                             7 \d)})
                              (select-keys counts b-and-d))) ;; use occurrence counts to figure out the remaining
                      )
                  )]

    (->> outputs
         (map (fn [output] (digits (set (map #(get mapping %) output)))))
         str/join
         Integer/parseInt)))

(defn part2 []
  (reduce + (map decode-row input)))

(comment
  (let [sample (parse-input  "inputs/aoc_2021/input_day8_sample.txt")]
    (reduce + (map decode-row sample)))
  (part1)
  (part2))