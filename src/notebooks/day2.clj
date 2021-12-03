^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day2
  (:require [me.noonian.aoc-2021.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 2)

;; ## Part 1

(def input (util/input-for "day2"))

;; The input represents a list of instructions consisting of a
;; direction and a value (an integer). I'm representing each
;; instruction as a map with keys `:dir` and `:value`. I chose to make
;; the direction a keyword instead of a string because they are a
;; little nicer to work with.

;; I wasted a little time because I initially forgot to parse the
;; value into a number.

(def parsed-input
  (for [[dir n] (map #(str/split % #" ") (str/split-lines input))]
    {:dir (keyword dir)
     :value (Long/parseLong n)}))

;; I'll model the submarine state with a map, with keys `:pos`
;; and `:depth` initialized to `0`.

(def initial-state {:pos 0 :depth 0})

;; Now we need a function that takes the current state and an
;; instruction and returns an updated state. I'll call this function
;; `move`.

(defn move [state {:keys [dir value]}]
  (condp = dir
    :forward (update state :pos + value)
    :down (update state :depth + value)
    :up (update state :depth - value)))

;; To get the final state, we can reduce our input using the move
;; function and our initial state.

(def part1 (reduce move initial-state parsed-input))

;; Then we get the answer by multiplying the position by the depth

(* (:pos part1) (:depth part1))

;; ### Visualizing intermediate states

;; We can get a sequence of each state along the submarines journey by
;; using `reductions` instead of `reduce`.

(def part1-states (reductions move initial-state parsed-input))

;; We can use Clerk's vega-lite viewer to render the states as a line
;; chart. Figuring out the the input options for vega-lite proved as
;; hard as todays advent challenge.

(clerk/vl
 {:data {:values part1-states}
  :mark {:type "line" :interpolate "step-after"}
  :encoding {:x {:field :pos :type "quantitative"}
             :y {:field :depth :type "quantitative"}}
  :height 400 :width 700})

;; ## Part 2

;; Part 2 alters the semantics of the instructions, and introduces a
;; new bit of state called `:aim`.

;; Let's make a new `move2` function that implements the new instruction
;; logic.

(defn move2 [{:keys [aim] :as state} {:keys [dir value]}]
  (condp = dir
    :forward (-> state
                 (update :pos + value)
                 (update :depth + (* aim value)))
    :down (update state :aim + value)
    :up (update state :aim - value)))

;; Reducing with this new function (and new `:aim` state initialized
;; to `0`) gives us the final state of part 2.

(def part2 (reduce move2 (assoc initial-state :aim 0) parsed-input))

;; And again, multiplying the position and depth give us the answer.

(* (:pos part2) (:depth part2))

;; ### Visualizing intermediate states

(def part2-states (reductions move2 (assoc initial-state :aim 0) parsed-input))

(clerk/vl
 {:data {:values part2-states}
  :mark {:type "line" :interpolate "step-after"}
  :encoding {:x {:field :pos :type "quantitative"}
             :y {:field :depth :type "quantitative"}}
  :height 400 :width 700})
