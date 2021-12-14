^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day13
  (:require [me.noonian.aoc-2021.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 13)

(def input (util/input-for "day13"))

(defn parse-instructions [s]
  (let [[dots fold-instructions] (str/split s #"\n\n")]
    {:dots (into #{}
             (for [s (str/split-lines dots)
                   :let [[x y] (str/split s #",")]]
               [(Long/parseLong x) (Long/parseLong y)]))
     :instructions (into []
                     (for [s (str/split-lines fold-instructions)
                           :let [[_ axis val] (re-find #"fold along (x|y)=(\d+)" s)]]
                       [(keyword axis) (Long/parseLong val)]))}))

(re-find #"fold along (x|y)=(\d+)" "fold along x=7")

;; I'm representing the dots as a set of [x y] vectors (points)

(def ex
  (parse-instructions
   "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(defn fold-up [fold-y [x y]]
  (let [delta (- y fold-y)]
    (if (neg? delta)
      [x y]
      [x (- fold-y delta)])))

(defn fold-left [fold-x [x y]]
  (let [delta (- x fold-x)]
    (if (neg? delta)
      [x y]
      [(- fold-x delta) y])))

(defn as-str [points]
  (let [x-max (apply max (map first points))
        y-max (apply max (map second points))]
    (str/join "\n"
      (for [y (range 0 (inc y-max))]
        (str/join
            (for [x (range 0 (inc x-max))]
              (if (points [x y]) "#" ".")))))))

(def display (comp println as-str))

(defn fold [points [axis val]]
  (let [fold-point (if (= :x axis)
                     (partial fold-left val)
                     (partial fold-up val))]
    (into #{}
      (map fold-point points))))

(clerk/code
 (as-str
  (-> (:dots ex)
      (fold (first (:instructions ex)))
      (fold (second (:instructions ex))))))

;; ## Part 1

(let [{:keys [dots instructions]} (parse-instructions input)]
  (count
   (fold dots (first instructions))))

;; ## Part 2

(let [{:keys [dots instructions]} (parse-instructions input)]
  (clerk/code
   (as-str (reduce fold dots instructions))))
