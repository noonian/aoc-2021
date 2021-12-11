^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day6
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 6)

(def input (util/input-for "day6"))

(defn parse-fish-ages [s]
  (mapv #(Long/parseLong %)
        (str/split s #",")))

(defn dec-mod [val n]
  (if (zero? val) n (dec val)))

(defn simulate-day [timers]
  (let [num-new (count (filter zero? timers))]
    (into (map #(dec-mod % 6) timers)
      (repeat num-new 8))))

;; ## Part 1

;; You can visualize the intermediate state of the example input
;; nicely with `iterate`

(->> (parse-fish-ages "3,4,3,1,2")
     (iterate simulate-day)
     (take 4))

;; Calculating the answer for the example input

(->> (parse-fish-ages "3,4,3,1,2")
     (iterate simulate-day)
     (drop 80)
     first
     count)

(->> (parse-fish-ages input)
     (iterate simulate-day)
     (drop 80)
     first
     count)

;; ## Part 2

;; Part 2 is much harder. Because the function is likely exponential,
;; it takes too long to compute 256 days ahead naively. At first I
;; spent some time trying to figure out an exact mathematical
;; expression for growth of the number of fish. However, I had an idea
;; of using a nice representation that might get us the performance we
;; need or give better insight into the correct expression.

;; Instead of modelling each timer separately, we can use a map with
;; the keys being the days on the timer 0-8 and the values being the
;; number of fish with those timer values. This should speed things up
;; a lot because we have a constant set of numbers and can caluclate
;; the totals via counting by the values.

(def safe-plus (fnil +' 0))

(defn simulate-day2 [timers]
  (let [num-new (get timers 0 0)]
    (-> (into {}
          (for [[timer n] timers
                :when (not (zero? timer))]
            [(dec timer) n]))
        (update 6 safe-plus num-new)
        (update 8 safe-plus num-new))))

(->> (parse-fish-ages "3,4,3,1,2")
     frequencies
     (iterate simulate-day2)
     (drop 256)
     first
     vals
     (apply +))

;; I'm very happy with this approach! It is much faster, pretty easy
;; to reason about, and has constant space and linear time in the
;; number of days.

(->> (parse-fish-ages input)
     frequencies
     (iterate simulate-day2)
     (drop 256)
     first
     vals
     (apply +'))

;; How fast can we calculate 1,000,000 days?

(time
 (->> (parse-fish-ages input)
      frequencies
      (iterate simulate-day2)
      (drop 1000000)
      first
      vals
      (apply +')))

;; ~5.6 seconds

;; This does actually produce an answer but it is a BigInteger larger
;; than Long/MAX_VALUE and it appears that Clerk cannot render it.
