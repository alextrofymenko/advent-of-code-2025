(ns core
  (:require [clojure.string :as str]))

(def sample
  ["987654321111111"
   "811111111111119"
   "234234234234278"
   "818181911112111"])
(def input (str/split-lines (slurp "03/input.txt")))

(declare max-joltage)

(defn- max-joltage-given-battery-1 [bank battery-1-index total-batteries]
  (when (and (>= battery-1-index 0)
             (< (+ battery-1-index (dec total-batteries)) (count bank)))
    (let [battery-1-joltage (get bank battery-1-index)]
      (if (> total-batteries 2)
        (str battery-1-joltage (max-joltage (subs bank (inc battery-1-index)) (dec total-batteries)))
        (let [battery-2-joltage (apply max (map int (subs bank (inc battery-1-index))))]
          (str battery-1-joltage (char battery-2-joltage)))))))

(defn- max-joltage
  "If there are any battery combinations starting 9, just return those,
   since they will be higher than any other batteries (there may be more
   than one though, so check them all)"
  [bank total-batteries]
  (loop [jotage-values (apply str (range 9 0 -1))]
    (when (seq jotage-values)
      (or (some->> (map-indexed vector bank)
                   (keep (fn [[i j]]
                           (when (= j (first jotage-values))
                             (max-joltage-given-battery-1 bank i total-batteries))))
                   (map parse-long)
                   (seq)
                   (apply max))
          (recur (rest jotage-values))))))

(defn part-1 [input]
  (apply + (map #(max-joltage % 2) input)))

(defn part-2 [input]
  (apply + (map #(max-joltage % 12) input)))
