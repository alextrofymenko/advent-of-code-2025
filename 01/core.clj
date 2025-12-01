(ns core
  (:require
   [clojure.string :as str]))

(def sample ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"])
(def input (str/split-lines (slurp "01/input.txt")))

(defn- normalize [result]
  (loop [r result]
    (let [r* (cond
               (< r 0) (- 100 (Math/abs r))
               (> r 99) (- r 100)
               (= r 100) 0
               :else r)]
      (if (<= 0 r* 99)
        r*
        (recur r*)))))

(defn- rotate [pos [dir & clicks]]
  (let [clicks (parse-long (apply str clicks))
        result ((if (= \R dir) + -) pos clicks)]
    (normalize result)))

(defn part-1 [input]
  (loop [position  50
         rotations input
         zeros     0]
    (if-let [rotation (first rotations)]
      (let [pos (rotate position rotation)]
        (recur pos (rest rotations) (cond-> zeros (zero? pos) inc)))
      zeros)))
