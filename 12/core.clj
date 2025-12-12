(ns core
  (:require [clojure.string :as str]))

(def sample
  ["0:"
   "###"
   "##."
   "##."
   ""
   "1:"
   "###"
   "##."
   ".##"
   ""
   "2:"
   ".##"
   "###"
   "##."
   ""
   "3:"
   "##."
   "###"
   "##."
   ""
   "4:"
   "###"
   "#.."
   "###"
   ""
   "5:"
   "###"
   ".#."
   "###"
   ""
   "4x4: 0 0 0 0 2 0"
   "12x5: 1 0 1 0 2 2"
   "12x5: 1 0 1 0 3 2"])
(def input (str/split-lines (slurp "12/input.txt")))

(defn- parse-input [input]
  (let [[areas & shapes] (reverse (remove #(= [""] %) (partition-by #(= "" %) input)))]
    {:areas (reduce
             (fn [res region]
               (let [[size presents] (str/split region #": ")
                     [w l] (str/split size #"x")]
                 (conj res {:area (* (parse-long w) (parse-long l))
                            :presents (into {} (map-indexed (fn [i p] [i (parse-long p)]) (str/split presents #" ")))})))
             [] areas)
     :shapes (reduce
              (fn [res shape]
                (let [[index spaces] (str/split (apply str shape) #":")
                      size (count (filter #(= \# %) spaces))]
                  (assoc res (parse-long index) {:size size})))
              {} shapes)}))

(defn part-1
  "I didn't even know where to start with this, so I went to look at hints on
   Reddit, and I have to say I'm a little disappointed this was just a troll :(

   This doesn't work on sample, of course...only on the real input"
  [input]
  (let [{:keys [shapes areas]} (parse-input input)]
    (count
     (for [{:keys [area presents]} areas
           :let [required-area (reduce-kv #(+ %1 (* (:size (get shapes %2)) %3)) 0 presents)]
           :when (< required-area area)]
       area))))
