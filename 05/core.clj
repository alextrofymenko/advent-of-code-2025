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

(defn- ranges-overlap?
  "Case 1                 Case 2
   range-1: |-----|       range-1:    |-----|
   range-2:    |-----|    range-2: |-----|

   Case 3                 Case 4
   range-1: |-------|     range-1:   |---|
   range-2:   |---|       range-2: |-------|"
  [range-1 range-2]
  (or (<= (:min range-1) (:min range-2) (:max range-1))                ;; Case 1
      (<= (:min range-2) (:min range-1) (:max range-2))                ;; Case 2
      (<= (:min range-1) (:min range-2) (:max range-2) (:max range-1)) ;; Case 3
      (<= (:min range-2) (:min range-1) (:max range-1) (:max range-2)) ;; Case 4
      ))

(defn- expanded-ranges [db]
  (reduce
   (fn [expanded-ranges range-1]
     (if-let [overlapping-ranges (seq (filter #(ranges-overlap? range-1 (second %)) expanded-ranges))]
       (-> (apply dissoc expanded-ranges (map first overlapping-ranges))
           (assoc (random-uuid) {:min (apply min (:min range-1) (map (comp :min second) overlapping-ranges))
                                 :max (apply max (:max range-1) (map (comp :max second) overlapping-ranges))}))
       (assoc expanded-ranges (random-uuid) range-1)))
   {} (:fresh-ranges db)))

(defn part-2 [input]
  (reduce-kv #(+ %1 (- (:max %3) (:min %3)) 1) 0 (expanded-ranges (read-db input))))
