^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day3
  (:require [me.noonian.aoc-2021.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 3)

;; This is quick and dirty and I haven't cleaned things up. I kept the
;; numbers as strings so I could get the digits easily, but I'd like
;; to switch to longs eventually.

;; ## Part 1

(def input (util/input-for "day3"))

(def parsed-input
  (str/split-lines (str/trim input)))

(defn most-common-bit [digit strs]
  (let [freqs (frequencies (mapv #(nth % digit) strs))]
    (when-not (and (> (count freqs) 1)
                   (apply = (vals freqs)))
      (->> (frequencies (mapv #(nth % digit) strs))
           (reduce (fn [[char num] [c n]]
                     (if (> n num)
                       [c n]
                       [char num]))
                   [nil 0])
           first))))

(defn least-common-bit [digit strs]
  (let [freqs (frequencies (mapv #(nth % digit) strs))]
    (when-not (and (> (count freqs) 1)
                   (apply = (vals freqs)))
      (->> freqs
           (reduce (fn [[char num] [c n]]
                     (if (< n num)
                       [c n]
                       [char num]))
                   [nil Long/MAX_VALUE])
           first))))

(defn gamma [num-strs]
  (->> (for [i (range 12)]
         (most-common-bit i num-strs))
       (apply str)))

(defn epsilon [num-strs]
  (->> (for [i (range 12)]
         (least-common-bit i num-strs))
       (apply str)))

(defn power-consumption [report]
  (* (Long/parseLong (gamma report) 2)
     (Long/parseLong (epsilon report) 2)))

(power-consumption parsed-input)

;; ## Part 2

(defn oxygen-bit-criteria [index num-strs]
  (let [val (or (most-common-bit index num-strs) \1)]
    (filter #(= val (nth % index)) num-strs)))

(defn co2-scrubber-bit-criteria [index num-strs]
  (let [val (or (least-common-bit index num-strs) \0)]
    (filter #(= val (nth % index)) num-strs)))

(defn find-rating [bit-criteria report]
  (loop [i 0
         nums report]
    (if (= 1 (count nums))
      (first nums)
      (recur (inc i)
             (bit-criteria i nums)))))

(defn oxygen-rating [report]
  (find-rating oxygen-bit-criteria report))

(oxygen-rating parsed-input)

(defn co2-scrubber-rating [report]
  (find-rating co2-scrubber-bit-criteria report))

(defn life-support-rating [report]
  (* (Long/parseLong (oxygen-rating report) 2)
     (Long/parseLong (co2-scrubber-rating report) 2)))

(life-support-rating parsed-input)
