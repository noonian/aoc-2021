^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day11
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]
            [clojure.spec.alpha :as s]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 11)

(def input (util/input-for "day11"))

;; I decided to model the state as a map from points to energy levels,
;; instead of how I did it in day9 as just nested vectors. This makes
;; parsing a little harder, but the hope is that manipulating the data
;; is easier.

(defn add-row [m y s]
  (reduce (fn [res [x c]]
            (let [n (Long/parseLong (str c))]
              (assoc res [x y] n)))
          m
          (map-indexed vector s)))

(defn parse-octopuses [s]
  (reduce (fn [res [y s]] (add-row res y s))
          {}
          (map-indexed vector (str/split-lines s))))

(def ex
  (parse-octopuses "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

;; Instead of making a new bespoke neighbors function like in day 9, I
;; decided to make a factory function that will create neighbors
;; functions based on some parameters like whether or not to include
;; diaganals and the bounds of the space.

;; This way, if we see more problems requiring similar logic I should
;; be able to use or adapt this function.

(defn in-bounds-pred [[x-min x-max] [y-min y-max]]
  (fn [[x y]]
    (and (<= x-min x x-max)
         (<= y-min y y-max))))

(defn neighbors-fn [{:keys [x-bounds
                            y-bounds
                            diag?]}]
  (let [in-bounds? (in-bounds-pred x-bounds y-bounds)]
    (fn [[x y]]
      (->> (cond-> #{[(dec x) y]
                     [(inc x) y]
                     [x (inc y)]
                     [x (dec y)]}
             diag? (set/union #{[(dec x) (dec y)]
                                [(dec x) (inc y)]
                                [(inc x) (dec y)]
                                [(inc x) (inc y)]}))
           (filter in-bounds?)
           (into #{})))))

(def neighbors
  (neighbors-fn {:x-bounds [0 9]
                 :y-bounds [0 9]
                 :diag? true}))

(defn perform-flashes [m]
  (loop [res m]
    (if (every? #(<= % 9) (vals res))
      res
      (let [to-flash (map first (filter #(> (second %) 9) res))]
        (recur (reduce (fn [m p]
                         (let [adjacent (neighbors p)]
                           (reduce (fn [m p]
                                     (update m p (fn [energy]
                                                   (cond-> energy
                                                     (not (zero? energy)) (inc)))))
                                   (assoc m p 0)
                                   adjacent)))
                       res
                       to-flash))))))

(defn evolve-step [m]
  (perform-flashes (update-vals m inc)))

(defn evolve-step' [{:keys [state num-flashed]}]
  (let [new-state (evolve-step state)]
    {:state new-state
     :num-flashed (+ num-flashed (count (filter zero? (vals new-state))))}))

(:num-flashed
 (evolve-step'
  (evolve-step' {:state ex :num-flashed 0})))

(->> {:state ex :num-flashed 0}
     (iterate evolve-step')
     (drop 10)
     first
     :num-flashed)

(->> {:state ex :num-flashed 0}
     (iterate evolve-step')
     (drop 100)
     first
     :num-flashed)

;; ## Part 1

(->> {:state (parse-octopuses input) :num-flashed 0}
     (iterate evolve-step')
     (drop 100)
     first
     :num-flashed)

;; ## Part 2

;; Part 2 should be relatively easy because our approach already
;; generates a seq of states. So if we just take them until they are
;; finished and count them we should have the answer.

;; Note that take-while will return everything *before* the finished
;; state, but the first item in the collection will be the initial
;; state so count should give us the correct answer without any off by
;; one errors.

(defn finished? [{:keys [state]}]
  (every? zero? (vals state)))

(->> {:state (parse-octopuses input) :num-flashed 0}
     (iterate evolve-step')
     (take-while (complement finished?))
     count)
