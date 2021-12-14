^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day14
  (:require [me.noonian.aoc-2021.util :as util]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 14)

(def input (util/input-for "day14"))

;; I had to change my approach for part 2, but I'm leaving the
;; original code of part 1 for posterity

(defn parse [s]
  (let [[template rules] (str/split s #"\n\n")]
    {:template (map str (seq template))
     :rules (->> (str/split-lines rules)
                 (map #(re-find #"(.*) \-\> (.*)" %))
                 (map rest)
                 (map #(into [] %))
                 (into {}))}))

(defn evolve [{:keys [rules template] :as base}]
  (let [pairs (partition 2 1 template)
        most (->> (for [[a b] pairs
                        :let [el (str (get rules (str a b)))]]
                    [a el])
                  (apply concat)
                  (into []))]
    (assoc base :template (conj most (get template (dec (count template)))))))

(def ex
  (parse "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))

(evolve ex)

(defn solve [{:keys [template n]}]
  (let [sorted-freqs (freqs template)]
    (-' (second (last sorted-freqs))
        (second (first sorted-freqs)))))

(->> (iterate evolve ex)
     (drop 10)
     first
     solve)

(time
 (->> (iterate evolve (parse input))
      (drop 10)
      first
      solve))

;; ## Part 2

;; After wasting a bunch of time with a different approach, I had the
;; realization that each pair transforms into two new pairs. And, we
;; can keep track of the number of each unique pair without caring
;; about the order in the template. We need to remember the last char
;; for counting occurances later since the pairs duplicate chars (the
;; first char in some pairs is duplicated by the second char in
;; others).

(defn parse2 [s]
  (let [[template rules] (str/split s #"\n\n")]
    {:template (frequencies (partition 2 1 template))
     :last-char (last template)
     ;; This is messy, with some extra work to ensure everything is
     ;; chars and not strings.
     :rules (->> (str/split-lines rules)
                 (map #(re-find #"(.*) \-\> (.*)" %))
                 (map rest)
                 (map (fn [[a b]] [(into [] a) (first (seq b))]))
                 (map (fn [[[a b] c]]
                        [[a b] [[a c] [c b]]]))
                 (into {}))}))

(defn evolve3 [{:keys [template rules] :as base}]
  (let [counts (mapcat (fn [[pair count]]
                         (let [new-pairs (get rules pair)]
                           (for [p new-pairs]
                             [p count])))
                       template)]
    (assoc base :template
           (reduce (fn [freqs [pair count]]
                     (update freqs pair (fnil +' 0) count))
                   {}
                   counts))))

(defn solve2 [{:keys [template last-char]}]
  (let [char-counts (reduce-kv (fn [res pair n]
                                 (update res (first pair) (fnil +' 0) n))
                               {last-char 1}
                               template)
        sorted (sort-by second char-counts)]
    (-' (second (last sorted))
        (second (first sorted)))))

(def ex2
  (parse2 "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))

(time
 (->> ex2
      (iterate evolve3)
      (drop 40)
      first
      solve2))

(time
 (->> (parse2 input)
      (iterate evolve3)
      (drop 40)
      first
      solve2))
