(ns core
  (:require [clojure.string :as str]))

(def sample
  [".......S......."
   "..............."
   ".......^......."
   "..............."
   "......^.^......"
   "..............."
   ".....^.^.^....."
   "..............."
   "....^.^...^...."
   "..............."
   "...^.^...^.^..."
   "..............."
   "..^...^.....^.."
   "..............."
   ".^.^.^.^.^...^."
   "..............."])
(def input (str/split-lines (slurp "07/input.txt")))

(defn- replace-row [manifold n row]
  (let [[before after] (split-at (inc n) manifold)]
    (vec (concat before [row] (rest after)))))

(defn- process-frame [{:keys [manifold splits]} n]
  (when (< (- n 2) (count manifold))
    (let [beam-positions (reduce-kv #(cond-> %1 (contains? #{\S \|} %3) (conj %2)) [] (vec (nth manifold n)))
          {:keys [next splits]} (reduce
                                 (fn [{:keys [next] :as state} position]
                                   (if (= (nth next position) \^)
                                     (-> (update state :splits inc)
                                         (assoc :next (str (when (> position 0)
                                                             (str (subs next 0 (dec position)) \|))
                                                           "^"
                                                           (when (< (inc position) (count next))
                                                             (str "|" (subs next (+ position 2) (count next)))))))
                                     (assoc state :next (str (subs next 0 position) "|" (subs next (inc position) (count next))))))
                                 {:next (nth manifold (inc n)) :splits splits} beam-positions)]
      {:manifold (replace-row manifold n next) :splits splits})))

(defn- process-manifold [manifold]
  (reduce #(process-frame %1 %2) {:manifold manifold :splits 0} (range (dec (count manifold)))))

(defn part-1 [input]
  (:splits (process-manifold input)))
