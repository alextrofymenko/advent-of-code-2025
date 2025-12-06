(ns core
  (:require [clojure.string :as str]))

(def sample
  ["123 328  51 64 "
   " 45 64  387 23 "
   "  6 98  215 314"
   "*   +   *   +  " ;; SPACES ARE IMPORTANT!
   ])
;; Make sure your editor doesn't trim end-of-line spaces!!!
(def input (str/split-lines (slurp "06/input.txt")))

(def ->fn
  "In the interest of being safe, though would have been
   even neater with read-string & eval"
  {"*" * "+" +})

(defn- read-like-a-human [input]
  (let [nums (map #(str/split (str/trim %) #"\s+") (butlast input))]
    (for [[i f] (map-indexed vector (str/split (last input) #"\s+"))]
      (str/join " " (concat [f] (map #(nth % i) nums))))))

(defn- read-like-a-cephalopod [input]
  (loop [[fn-str :as fns] (reverse (re-seq #"[*+]\s+" (last input)))
         cursor (count (last input))
         equations []]
    (if fn-str
      (let [func (str/trim fn-str)
            nums (for [c (range (count fn-str))]
                   (apply str (map #(nth % (- (dec cursor) c)) (butlast input))))]
        (recur (rest fns) (- cursor (count fn-str)) (conj equations (str/join " " (cons func nums)))))
      equations)))

(defn- solve-equations [equations]
  (reduce
   (fn [grand-total equation]
     (let [[f & nums] (str/split equation #"\s+")]
       (+ grand-total (apply (->fn f) (map parse-long nums)))))
   0 equations))

(defn part-1 [input]
  (solve-equations (read-like-a-human input)))

(defn part-2 [input]
  (solve-equations (read-like-a-cephalopod input)))
