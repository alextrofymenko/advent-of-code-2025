(ns core
  (:require [clojure.string :as str]))

(def sample
  ["123 328  51 64 "
   " 45 64  387 23 "
   "  6 98  215 314"
   "*   +   *   +"])
(def input (str/split-lines (slurp "06/input.txt")))

(def ->fn
  "In the interest of being safe, though would have been
   even neater with read-string & eval"
  {"*" * "+" +})

(defn part-1 [input]
  (let [[fns & nums] (reverse input)
        nums (map #(str/split (str/trim %) #"\s+") nums)]
    (reduce
     (fn [grand-total [i f]]
       (let [terms (map (comp parse-long #(nth % i)) nums)]
         (+ grand-total (apply (->fn f) terms))))
     0 (map-indexed vector (str/split (str/trim fns) #"\s+")))))
