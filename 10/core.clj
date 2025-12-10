(ns core
  (:require [clojure.string :as str]))

(def sample
  ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
   "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
   "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"])
(def input (str/split-lines (slurp "10/input.txt")))

(defn- decode-machine [line]
  (let [[_ lights buttons _joltage-reqs] (re-find #"^\[(.*?)\] (.*?) \{(.*?)\}$" line)]
    {:lights       (mapv #(= \# %) lights)
     :buttons      (mapv #(mapv parse-long (str/split % #",")) (re-seq #"(?<=\().*?(?=\))" buttons))}))

(defn- press-button [state button]
  (reduce #(update %1 %2 not) state button))

(defn- solve-machine [{:keys [lights buttons]}]
  (let [empty-state (vec (repeat (count lights) false))]
    (loop [button-seqs #{#{}}]
      (or (first
           (keep (fn [button-seq]
                   (when (= lights (reduce press-button empty-state (map #(get buttons %) button-seq)))
                     button-seq)) button-seqs))
          (recur
           (reduce-kv
            (fn [res i _button]
              (reduce #(conj %1 (conj %2 i)) res button-seqs))
            #{} buttons))))))

(defn part-1 [input]
  (apply + (map (comp count solve-machine decode-machine) input)))
