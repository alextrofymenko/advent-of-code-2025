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

(defn part-1 [input]
  (let [g (grid input)]
    (count
     (for [[[x y] _] g
           :when (= \@ (get g [x y]))
           :let [ns (keep g (neighbours x y))]
           :when (< (count (filter #(= \@ %) ns)) 4)]
       [x y]))))
