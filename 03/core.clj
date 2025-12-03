(ns core
  (:require [clojure.string :as str]))

(def sample
  ["987654321111111"
   "811111111111119"
   "234234234234278"
   "818181911112111"])
(def input (str/split-lines (slurp "03/input.txt")))

(defn- max-joltage-given-battery-1 [bank battery-1-index]
  (when (and (>= battery-1-index 0)
             (< (inc battery-1-index) (count bank)))
    (let [battery-1-joltage (get bank battery-1-index)
          battery-2-joltage (apply max (map int (subs bank (inc battery-1-index))))]
      (str battery-1-joltage (char battery-2-joltage)))))

(defn- max-joltage
  "If there are any battery combinations starting 9, just return those,
   since they will be higher than any other batteries (there may be more
   than one though, so check them all)"
  [bank]
  (loop [jotage-values (apply str (range 9 0 -1))]
    (when (seq jotage-values)
      (or (some->> (map-indexed vector bank)
                   (keep (fn [[i j]]
                           (when (= j (first jotage-values))
                             (max-joltage-given-battery-1 bank i))))
                   (map parse-long)
                   (seq)
                   (apply max))
          (recur (rest jotage-values))))))

(defn part-1 [input]
  (apply + (map max-joltage input)))
