^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day9
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 9)

(def input (util/input-for "day9"))

(def ex
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-heightmap [s]
  (->> (str/split-lines s)
       (mapv (fn [s] (mapv #(Long/parseLong (str %)) s)))))

(defn out-of-bounds? [[x y]]
  (or (neg? x)
      (neg? y)))

(defn neighbors [[x y]]
  (->> #{[(dec x) y]
         [(inc x) y]
         [x (inc y)]
         [x (dec y)]}
       (remove out-of-bounds?)
       (into #{})))

(neighbors [0 0])
(neighbors [2 3])

(defn height-at [hmap [x y]]
  (get-in hmap [y x]))

(defn risk-level [hmap point]
  (inc (height-at hmap point)))

;; hack for out of bounds points
(defn lower? [a b]
  (if-not b
    true
    (< a b)))

(defn low-point? [hmap point]
  (let [height (height-at hmap point)]
    (every? #(lower? height (height-at hmap %))
            (neighbors point))))

(defn low-points [hmap]
  (let [x-max (count (first hmap))
        y-max (count hmap)]
    ;; [x-max y-max]
    (into #{}
      (for [x (range x-max)
            y (range y-max)
            :when (low-point? hmap [x y])]
        [x y]))))

(low-point? (parse-heightmap ex) [0 4])

(low-points (parse-heightmap ex))

(let [hmap (parse-heightmap input)]
  (->> (map (partial risk-level hmap)
            (low-points hmap))
       (apply +)))

;; Part 2

(defn in-basin? [hmap min-height point]
  (when-let [height (height-at hmap point)]
    (and (< height 9)
         (> height min-height))))

(defn basin-points [hmap low-point]
  (loop [basin #{}
         others #{low-point}]
    (if-not (seq others)
      basin
      (recur (set/union basin others)
             (->> (for [point others
                        :let [h (height-at hmap point)]
                        :when h]
                    (filter #(in-basin? hmap h %)
                            (neighbors point)))
                  (apply concat)
                  (into #{}))))))

(let [hmap (parse-heightmap ex)]
  (->> (low-points hmap)
       (map #(basin-points hmap %))
       (map count)
       (sort-by -)
       (take 3)
       (apply *)))

(let [hmap (parse-heightmap input)]
  (->> (low-points hmap)
       (map #(basin-points hmap %))
       (map count)
       (sort-by -)
       (take 3)
       (apply *)))
