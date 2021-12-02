^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day1
  (:require [me.noonian.aoc-2021.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 1)

;; I'm trying something new this year. My goal is to reduce the stress
;; of advent of code by taking my time more on each problem and
;; suppressing my natural competitiveness.

;; I plan on documenting my appraoch here
;; using [Clerk](https://github.com/nextjournal/clerk)

;; Clerk comes with many nifty visualization tools and is extensible
;; so learning how to use it may prove useful in the future.

(def input (util/input-for "day1"))

(def parsed-input (mapv #(Long/parseLong %) (str/split-lines input)))

;; ## Part 1

;; Part 1 asks us to count the number of times the numbers
;; increase. Clojure has a handy function called `partition` that will
;; give us a sliding window of values. By filtering with the `<`
;; function we can get a list of all the windows that
;; increased. Counting that sequence will give us the answer to part
;; 1.

(->> (partition 2 1 parsed-input)
     (filter #(apply < %))
     count)

;; ## Part 2

;; Part 2 is very similar to part 1 except we need to first use a
;; sliding window of size 3, then sum the windows, then proceed with
;; the approach from part 1.

(->> (partition 3 1 parsed-input)
     (map #(apply + %))
     (partition 2 1)
     (filter #(apply < %))
     count)
