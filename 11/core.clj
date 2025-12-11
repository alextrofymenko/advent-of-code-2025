(ns core
  (:require [clojure.string :as str]))

(def sample
  ["aaa: you hhh"
   "you: bbb ccc"
   "bbb: ddd eee"
   "ccc: ddd eee fff"
   "ddd: ggg"
   "eee: out"
   "fff: out"
   "ggg: out"
   "hhh: ccc fff iii"
   "iii: out"])
(def input (str/split-lines (slurp "11/input.txt")))

(defn- map-devices [input]
  (reduce
   #(let [[in outs] (str/split %2 #": ")]
      (assoc %1 in (str/split outs #" ")))
   {} input))

(defn- map-paths [device-map start]
  (lazy-seq (cons start (map #(map-paths device-map %) (get device-map start)))))

(defn- part-1 [input]
  (->> (map-paths (map-devices input) "you")
       (flatten)
       (filter #(= "out" %))
       (count)))
