^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns notebooks.day10
  (:require [me.noonian.aoc-2021.util :as util]
            [me.noonian.aoc-2021.lib :as lib]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]
            [clojure.spec.alpha :as s]))

^{::clerk/visibility :hide}
(util/aoc-day-heading 10)

(def input (util/input-for "day10"))

(def ex-lines (str/split-lines "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))


(def delimiters
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def opening-delimeter? (set (keys delimiters)))
(def closing-delimeter? (set (vals delimiters)))

(def scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn parse-chunk
  "Returns [chunk, rest-of-input-string]"
  [s]
  (loop [c (first s)
         others (next s)
         open-chunks '()]
    (cond
      (and (not c)
           (seq open-chunks))
      [:incomplete open-chunks]

      (not c) :complete

      (and (closing-delimeter? c)
           (not= c (get delimiters (first open-chunks))))
      (throw (ex-info "Invalid chunk" {:invalid-char c}))

      (opening-delimeter? c)
      (recur (first others) (next others) (cons c open-chunks))

      (closing-delimeter? c)
      (recur (first others) (next others) (rest open-chunks))

      :else (throw (ex-info "Unhandled case" {:char c})))))

(parse-chunk (first ex-lines))

(defn validate-chunks [chunks]
  (let [invalid-chars (atom {})]
    (doseq [chunk chunks]
      (try (parse-chunk chunk)
           (catch clojure.lang.ExceptionInfo e
             (let [{:keys [invalid-char]} (ex-data e)]
               (swap! invalid-chars update invalid-char (fnil inc 0))
               :invalid-chunk))))
    @invalid-chars))

(defn score [invalid-chars]
  (->> (for [[c n] invalid-chars]
         (* n (get scores c)))
       (apply +)))

(score
 (validate-chunks ex-lines))

(score
 (validate-chunks (str/split-lines input)))

;; Part 2

(defn open-chunks [line]
  (try
    (let [chunk (parse-chunk line)]
      (when (= :incomplete (first chunk))
        (second chunk)))
    (catch clojure.lang.ExceptionInfo e)))

(defn completion-string [open-chunks]
  (->> (for [c open-chunks]
         (get delimiters c))
       (apply str)))

(def completion-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-completion [s]
  (reduce (fn [score c] (+ (* 5 score)
                           (get completion-scores c)))
          0
          s))

(defn completion-strings [lines]
  (->> lines
       (map open-chunks)
       (filter identity)
       (map completion-string)))

(defn score-completions [completion-strings]
  (let [scores (->> completion-strings
                    (map score-completion)
                    (sort))
        [first-half second-half] (split-at (long (/ (count scores) 2)) scores)]
    (first second-half)))

(score-completions (completion-strings ex-lines))

(score-completions (completion-strings (str/split-lines input)))
