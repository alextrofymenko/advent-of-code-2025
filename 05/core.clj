(ns core
  (:require [clojure.string :as str]))

(def sample
  ["3-5"
   "10-14"
   "16-20"
   "12-18"
   ""
   "1"
   "5"
   "8"
   "11"
   "17"
   "32"])
(def input
  (str/split-lines (slurp "05/input.txt")))

(defn- read-db [input]
  (reduce
   #(if-let [[_ min max] (re-find #"(\d+)-(\d+)" %2)]
      (update %1 :fresh-ranges conj {:min (parse-long min) :max (parse-long max)})
      (update %1 :available conj (parse-long %2)))
   {:fresh-ranges [] :available #{}} input))

(defn part-1 [input]
  (let [{:keys [fresh-ranges available]} (read-db input)]
    (reduce
     (fn [sum id]
       (cond-> sum
         (some #(and id (>= id (:min %)) (<= id (:max %))) fresh-ranges) inc))
     0 available)))
