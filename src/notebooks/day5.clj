^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day5
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 5)

;; This one would have benefited from a library to do vector
;; arithmetic

;; ## Part 1

(def input (util/input-for "day5"))

(defn parse-line [s]
  (let [[x1 y1 x2 y2] (map #(Long/parseLong %)
                           (rest (re-find #"(\d+),(\d+) -> (\d+),(\d+)" s)))]
    [[x1 y1] [x2 y2]]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (mapv parse-line)))

(def lines
  (parse-input input))

(defn find-range [x1 x2]
  (if (> x1 x2)
    (range x1 (dec x2) -1)
    (range x1 (inc x2) 1)))

(defn horizontal? [[[x1 y1] [x2 y2]]]
  (= y1 y2))

(defn vertical? [[[x1 y1] [x2 y2]]]
  (= x1 x2))

(defn diagonal-points [[[x1 y1] [x2 y2]]]
  (into #{}
    (map vector
         (find-range x1 x2)
         (find-range y1 y2))))

(diagonal-points [[6 4] [2 0]])

(defn points* [[[x1 y1] [x2 y2] :as line]]
  (cond
    (= x1 x2) (set (for [y (find-range y1 y2)] [x1 y]))
    (= y1 y2) (set (for [x (find-range x1 x2)] [x y1]))
    :else (diagonal-points line)))

(def points (memoize points*))

(defn overlapping-points [l1 l2]
  (set/intersection (points l1)
                    (points l2)))

(defn overlapping-many [line other-lines]
  (apply set/union (map #(overlapping-points line %) other-lines)))

(defn overlapping [lines]
  (loop [lines lines
         res #{}]
    (if-not (seq lines)
      res
      (recur (rest lines)
             (set/union res (overlapping-many (first lines) (rest lines)))))))

(count
 (overlapping (filter (some-fn horizontal? vertical?) (parse-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))))

(count
 (overlapping
  (filter (some-fn horizontal? vertical?) lines)))

(time
 (count
  (overlapping lines)))
