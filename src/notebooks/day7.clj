^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day7
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 7)

;; I haven't cleaned this up

(def input (util/input-for "day7"))

(def s "16,1,2,0,4,2,7,1,2,14")

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split s #",")))

(def positions
  (mapv #(Long/parseLong %)
        (str/split s #",")))

(defn mean [nums]
  (long
   (Math/round
    (double
     (/ (reduce + 0 nums)
        (count nums))))))

(defn median [nums]
  (->> (frequencies nums)
       (sort-by (comp - second))
       ffirst))

(defn cost [target num]
  (Math/abs (- target num)))

(def triangle-number
  (memoize
   (fn [n]
     (loop [n n
            res 0]
       (cond
         (zero? n) res
         (= 1 n) (inc res)
         :else (recur (dec n)
                      (+ res n)))))))

(defn cost2 [target num]
  (triangle-number (Math/abs (- target num))))

(defn solve [cost-fn target nums]
  (for [n nums]
    {:target target
     :orig n
     :cost (cost-fn target n)}))

(str/split s #",")
(mean positions)

(defn brute-force [cost-fn nums]
  (let [avg (mean nums)
        m (median nums)
        lower (if (> avg m) m avg)
        upper (if (> avg m) avg m)
        solutions (map #(solve cost-fn % nums) (range lower upper))]
    (->>
     (for [s solutions]
       {:solution s
        :cost (reduce + 0 (map :cost s))
        :target (:target (first s))})
     (sort-by :cost)
     first)))

;; part 1
(time
 (dissoc
  (->> (parse input)
       (brute-force cost))
  :solution))

;; ~1.5 seconds

;; part 2

(time
 (dissoc
   (->> (parse input)
        (brute-force cost2))
   :solution))

;; ~1.7 seconds

;; Pretty slow over all
