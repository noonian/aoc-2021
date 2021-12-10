^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day8
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 8)

(def input (util/input-for "day8"))

(def digit->segments
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}
   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}
   8 #{:a :b :c :d :e :f :g}
   9 #{:a :b :c :d :f :g}})

(def segments->digit
  (into {}
    (for [[k v] digit->segments]
      [v k])))

(def unique-digits #{1 4 7 8})
(def unique-digit-segment-counts (mapv (comp count digit->segments) unique-digits))

(defn unique-digit-segments? [segments]
  (some #(= (count segments) %) unique-digit-segment-counts))

(def ex "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn parse-entry [s]
  (let [[left right] (str/split s #" \| ")
        patterns (str/split left #" ")
        output-digits (str/split right #" ")]
    {:signal-patterns
     (into []
       (for [s patterns]
         (into #{}
           (mapv #(keyword (str %)) s))))
     :output-digits
     (into []
       (for [s output-digits]
         (into #{}
           (mapv #(keyword (str %)) s))))}))

(parse-entry ex)

(defn parse-notes [s]
  (mapv parse-entry (str/split-lines s)))

(def ex2
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(->> (parse-notes ex2)
     (mapcat :output-digits)
     (filter unique-digit-segments?)
     count)

(->> (parse-notes input)
     (mapcat :output-digits)
     (filter unique-digit-segments?)
     count)

;; ## Part 2

(defn unique-segment-for [digit segments]
  (set (first
        (filter #(= (count %)
                    (count (digit->segments digit)))
                segments))))

;; So ugly...

(defn find-mapping [{:keys [signal-patterns output-digits]}]
  (let [c+f (unique-segment-for 1 signal-patterns)
        a+c+f (unique-segment-for 7 signal-patterns)
        a-dual (first (set/difference a+c+f c+f))
        b+d (set/difference (unique-segment-for 4 signal-patterns) c+f)
        eight (unique-segment-for 8 signal-patterns)
        three (first (filter #(and (set/subset? c+f (set %))
                                   (= 5 (count %)))
                             signal-patterns))
        e+g (set/difference eight c+f #{a-dual} b+d)
        g-dual (first (set/intersection three e+g))
        e-dual (first (set/difference e+g #{g-dual}))
        five (first (filter #(and (= 1 (count (set/intersection % c+f)))
                                  (= 5 (count %))
                                  (not= % three)
                                  (not (set/subset? #{e-dual} %)))
                            signal-patterns))
        f-dual (first (set/intersection five c+f))
        c-dual (first (set/difference c+f #{f-dual}))
        two (first (filter #(and (= 5 (count %))
                                 (not= % three)
                                 (not= % five))
                           signal-patterns))
        d-dual (first (set/difference two #{a-dual c-dual f-dual e-dual g-dual}))
        b-dual (first (set/difference b+d #{d-dual}))]
    {a-dual :a
     b-dual :b
     c-dual :c
     d-dual :d
     e-dual :e
     f-dual :f
     g-dual :g}))

(find-mapping (parse-entry ex))

(defn decode-digit [mapping digit-segments]
  (segments->digit (into #{} (map mapping digit-segments))))

(defn decode-output [mapping output-digits]
  (mapv #(decode-digit mapping %) output-digits))

(let [entry (parse-entry ex)]
  (decode-output (find-mapping entry) (:output-digits entry)))

(defn decode-entry [entry]
  (->> (decode-output (find-mapping entry) (:output-digits entry))
       (map str)
       (apply str)
       (Long/parseLong)))

(->> (parse-notes ex2)
     (map decode-entry)
     (apply +))

(->> (parse-notes input)
     (map decode-entry)
     (apply +))
