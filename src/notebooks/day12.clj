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
(util/aoc-day-heading 12)

(def input (util/input-for "day12"))

(defn parse-map [s]
  (-> (->> (str/split-lines s)
           (mapv #(mapv keyword (str/split % #"-")))
           (mapcat (fn [[k v]] [[k v] [v k]])) ;both directions
           (group-by first))
      ;; Prevent going to :start or leaving :end
      (update-vals #(into #{} (filter (complement #{:start}) (map second %))))
      (dissoc :end)))

(defn big? [k] (= (name k) (str/upper-case (name k))))
(def small? (complement big?))

;; I'm representing the map as a Map from cave to a set of exits from
;; that cave.

(def ex
  (parse-map "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))

(defn paths
  ([cmap] (paths cmap :start #{}))
  ([cmap c seen]
   (cond
     (and (seen c) (small? c)) []
     (= c :end) [[:end]]
     :else
     (let [exits (get cmap c)]
       (mapcat (fn [exit]
                 (for [path (paths cmap exit (conj seen c))]
                   (into [c] path)))
               exits)))))

(paths ex)
(count (paths ex))

(def ex2 (parse-map "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"))

(count (paths ex2))

(def ex3 (parse-map "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"))

(count (paths ex3))

;; ## Part 1

(count (paths (parse-map input)))

;; ## Part 2

(defn paths2
  ([cmap] (paths2 cmap :start {}))
  ([cmap c seen]
   (cond
     (and (small? c)
          (seen c)
          (some #(> % 1) (distinct (vals seen)))) []
     (= c :end) [[:end]]
     :else
     (let [exits (get cmap c)]
       (mapcat (fn [exit]
                 (for [path (paths2 cmap exit
                                    (cond-> seen
                                      (small? c) (update c (fnil inc 0))))]
                   (into [c] path)))
               exits)))))

(count (paths2 ex))
(count (paths2 ex2))
(count (paths2 ex3))
(count (paths2 (parse-map input)))
