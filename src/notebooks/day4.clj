^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day4
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 4)

;; ## Part 1

(def input (util/input-for "day4"))

(defn parse-order [s]
  (mapv #(Long/parseLong %) (str/split s #",")))

(defn parse-row [s]
  (into []
    (for [n (->>
             (str/split s #" ")
             (filter (complement str/blank?))
             (mapv #(Long/parseLong %)))]
      {:n n
       :marked? nil})))

(defn parse-board [s]
  (let [rows (str/split-lines s)]
    (mapv parse-row rows)))

(def order (parse-order (first (str/split-lines input))))

(defn parse-input [s]
  (let [lines (str/split-lines s)
        order (parse-order (first lines))
        boards
        (str/split
         (->> (drop 2 lines) (str/join (with-out-str (newline))))
         #"\n\n")]
    {:order order
     :boards (mapv parse-board boards)}))

(def parsed-input
  (parse-input input))

(first parsed-input)

(defn row [board n]
  (get board n))

(defn col [board n]
  (mapv #(get % n) board))

(col (parse-board " 3 82 18 50 90
16 37 52 67 28
30 54 80 11 10
60 79  7 65 58
76 83 38 51  1") 2)

(defn cols [board]
  (for [i (range 5)]
    (mapv #(get % i) board)))

(def rows identity)

(defn complete? [row-or-col]
  (every? :marked? row-or-col))

(defn wins? [board]
  (or (some complete? (rows board))
      (some complete? (cols board))))

(defn score [board num]
  (->> board
       (apply concat)
       (filter (complement :marked?))
       (map :n)
       (apply +)
       (* num)))

(defn mark [board n]
  (mapv (fn [row]
          (mapv (fn [cell]
                  (cond-> cell
                    (= n (:n cell)) (assoc :marked? true)))
                row))
        board))

(defn first-winning-board-score [{:keys [boards order]}]
  (loop [n nil
         nums order
         boards boards]
    (if-let [winning-board (first (filter wins? boards))]
      {:board winning-board
       :n n
       :score (score winning-board n)}
      (let [new-n (first nums)]
        (recur new-n
               (rest nums)
               (mapv #(mark % new-n) boards))))))

(defn last-winning-board-score [{:keys [boards order]}]
  (loop [n nil
         nums order
         boards boards]
    (if (and (= 1 (count boards))
             (wins? (first boards)))
      (let [last-winning-board (first boards)]
        {:board (first boards)
         :n n
         :score (score last-winning-board n)})
      (let [new-n (first nums)]
        (recur new-n
               (rest nums)
               (mapv #(mark % new-n)
                     (filterv (complement wins?)
                              boards)))))))

(defn show-board [board]
  (str/join "\n"
            (for [row board]
              (str/join " "
                        (for [{:keys [marked? n]} row]
                          (if marked?
                            "X"
                            n))))))


(:score (first-winning-board-score parsed-input))
(:score (last-winning-board-score parsed-input))

(:score (last-winning-board-score (parse-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")))
