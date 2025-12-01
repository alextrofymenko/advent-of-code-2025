(ns core
  (:require
   [clojure.string :as str]))

(def sample ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"])
(def input (str/split-lines (slurp "01/input.txt")))

(defn- rotate
  "Returns the number it will end up on, and the number of
   times it passed 0, in the form [num zero-passes]"
  [pos [dir & clicks]]
  (let [clicks (parse-long (apply str clicks))
        result ((if (= \R dir) + -) pos clicks)
        num (mod result 100)
        zero-passes (cond
                      (zero? result) 1                 ;; Ended up on zero is a trivial case
                      (pos? result)  (quot result 100) ;; Going up we just need to count 100s

                      ;; Going down is a bit tricky, because we need to count not only 100s,
                      ;; but also going below 0 counts, but only if we didn't start there
                      :else (cond-> (Math/abs (quot result 100))
                              (and (neg? result) (not (zero? pos))) inc))]
    [num zero-passes]))

(defn part-1 [input]
  (loop [position  50
         rotations input
         zeros     0]
    (if-let [rotation (first rotations)]
      (let [[pos] (rotate position rotation)]
        (recur pos (rest rotations) (cond-> zeros (zero? pos) inc)))
      zeros)))

(defn part-2 [input]
  (loop [position  50
         rotations input
         zeros     0]
    (if-let [rotation (first rotations)]
      (let [[pos zero-passes] (rotate position rotation)]
        (recur pos (rest rotations) (+ zeros zero-passes)))
      zeros)))
