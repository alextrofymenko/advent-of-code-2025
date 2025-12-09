(ns core
  (:require [clojure.string :as str]))

(def sample ["7,1" "11,1" "11,7" "9,7" "9,5" "2,5" "2,3" "7,3"])
(def input (str/split-lines (slurp "09/input.txt")))

(defn- coords [pos]
  (map parse-long (str/split pos #",")))

(defn- area [[x1 y1] [x2 y2]]
  (* (inc (if (> x1 x2) (- x1 x2) (- x2 x1)))
     (inc (if (> y1 y2) (- y1 y2) (- y2 y1)))))

(defn- rectangles [coords]
  (loop [[c1 :as coords] coords
         rectangles {}]
    (if-let [coords (next coords)]
      (recur coords (reduce #(assoc %1 [c1 %2] (area c1 %2)) rectangles coords))
      rectangles)))

(defn part-1 [input]
  (last (last (sort-by val (rectangles (map coords input))))))
