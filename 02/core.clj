(ns core
  (:require [clojure.string :as str]))

(def sample
  (str "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
       "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
       "824824821-824824827,2121212118-2121212124"))
(def input (str/trim (slurp "02/input.txt")))

(defn- expand-range [range-str]
  (let [[start end] (map parse-long (str/split range-str #"-"))]
    (range start (inc end))))

(defn- id-valid? [id]
  (let [str-id (str id)]
    (or (odd? (count str-id))
        (apply not= (split-at (/ (count str-id) 2) str-id)))))

(defn part-1 [input]
  (->> (str/split input #",")
       (mapcat expand-range)
       (remove id-valid?)
       (apply +)))
