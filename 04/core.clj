(ns core
  (:require [clojure.string :as str]))

(def sample
  ["..@@.@@@@."
   "@@@.@.@.@@"
   "@@@@@.@.@@"
   "@.@@@@..@."
   "@@.@@@@.@@"
   ".@@@@@@@.@"
   ".@.@.@.@@@"
   "@.@@@.@@@@"
   ".@@@@@@@@."
   "@.@.@@@.@."])

(def input (str/split-lines (slurp "04/input.txt")))

(defn- grid [input]
  (into {} (for [[y row] (map-indexed vector input)
                 [x pos] (map-indexed vector row)]
             [[x y] pos])))

(defn- neighbours [x y]
  (for [xd (range -1 2)
        yd (range -1 2)
        :when (not= 0 xd yd)]
    [(+ x xd) (+ y yd)]))

(defn- remove-rolls [grid]
  (reduce-kv
   (fn [[n res] [x y] p]
     (let [ns (keep grid (neighbours x y))
           can-remove? (and (= \@ (get grid [x y]))
                            (< (count (filter #(= \@ %) ns)) 4))]
       [(cond-> n can-remove? inc)
        (assoc res [x y] (if can-remove? \x p))]))
   [0 {}] grid))

(defn part-1 [input]
  (first (remove-rolls (grid input))))

(defn part-2 [input]
  (loop [g (grid input) n 0]
    (let [[n' g'] (remove-rolls g)]
      (if (pos? n')
        (recur g' (+ n n'))
        n))))
