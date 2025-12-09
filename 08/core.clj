(ns core
  (:require [clojure.string :as str]))

(def sample
  ["162,817,812"
   "57,618,57"
   "906,360,560"
   "592,479,940"
   "352,342,300"
   "466,668,158"
   "542,29,236"
   "431,825,988"
   "739,650,466"
   "52,470,668"
   "216,146,977"
   "819,987,18"
   "117,168,530"
   "805,96,715"
   "346,949,466"
   "970,615,88"
   "941,993,340"
   "862,61,35"
   "984,92,344"
   "425,690,689"])
(def input (str/split-lines (slurp "08/input.txt")))

(defn- coords [pos]
  (map parse-long (str/split pos #",")))

(defn- distance*
  "Not quite distance, but sqrt doesn't matter"
  [pos-1 pos-2]
  (+ (Math/pow (- (nth pos-1 0) (nth pos-2 0)) 2)
     (Math/pow (- (nth pos-1 1) (nth pos-2 1)) 2)
     (Math/pow (- (nth pos-1 2) (nth pos-2 2)) 2)))

(defn- sort-pairs-by-distances [coords]
  (loop [[position :as positions] coords
         distances {}]
    (if-let [positions (next positions)]
      (recur positions (reduce #(assoc %1 [position %2] (distance* position %2)) distances positions))
      (map first (sort-by val distances)))))

(defn- make-circuits
  "I misinterpreted the blurb and so it took me way longer than it should have -
   'After making the ten shortest connections' did not mean 'They used ten cables',
   it just means 'they examined ten shortest connections'. I wish that part was clearer"
  [coord-pairs {:keys [stop-after-examnining-n-pairs]}]
  (loop [number-of-attemps 0
         [coords & coord-pairs] coord-pairs
         circuits {}]
    (if (= number-of-attemps stop-after-examnining-n-pairs)
      circuits
      (let [[circuit-id-1 circuit-id-2]
            (for [c coords
                  group-id (reduce-kv #(cond-> %1 (contains? %3 c) (conj %2)) #{} circuits)]
              group-id)]
        (recur
         (inc number-of-attemps)
         coord-pairs
         (cond
           (and circuit-id-1 (= circuit-id-1 circuit-id-2))
           circuits

           (and circuit-id-1 circuit-id-2 (not= circuit-id-1 circuit-id-2))
           (-> (dissoc circuits circuit-id-2)
               (update circuit-id-1 into (get circuits circuit-id-2)))

           :else
           (update circuits (or circuit-id-1 circuit-id-2 (random-uuid)) (fnil into #{}) coords)))))))

(defn- estimate-size [{:keys [circuits]}]
  (->> (vals circuits)
       (map count)
       (sort)
       (take-last 3)
       (apply *)))

(defn- part-1 [input n]
  (let [sorted-pairs (sort-pairs-by-distances (map coords input))]
    (estimate-size (make-circuits sorted-pairs {:stop-after-examnining-n-pairs n}))))
