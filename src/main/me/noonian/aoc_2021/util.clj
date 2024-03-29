(ns me.noonian.aoc-2021.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as http]
            [hyperfiddle.rcf :refer [tests ! %]]
            [nextjournal.clerk :as clerk]))

(defn input-for [input-name]
  (str/trim-newline (slurp (io/resource (format "%s.txt" input-name)))))

(defn aoc-day-heading [day]
  (clerk/html [:div.viewer-markdown
               [:h1 [:a {:href (format "https://adventofcode.com/2021/day/%s" day)
                         :target "_blank"}
                     (format "Advent of Code - Day %s" day)]]]))

;; Shamelessly adapted from
;; https://github.com/Thomashrb/advent-of-code-data

(defn download-input [{:keys [token year day]}]
  (let [url (format "https://adventofcode.com/%s/day/%s/input" year day)]
    (http/get url {:headers {"Cookie" (format "session=%s" token)}})))

(defn fetch-input [{:keys [token year day outfile] :or {year 2021} :as opts}]
  (let [outfile (or outfile (format "day%s.txt" day))]
    (->> (download-input opts)
         :body
         (spit (format "resources/%s" outfile)))))

(tests

  (input-for "example-input") := "example input"

  )
