(ns user
  (:require [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [nextjournal.clerk :as clerk]
            [hyperfiddle.rcf]
            [me.noonian.aoc-2021.util :as util]))

(hyperfiddle.rcf/enable!)

(clerk/serve! {:browse? true
               :watch-paths ["src/notebooks" "src/main"]
               :show-filter-fn #(str/starts-with? % "src/notebooks")})

(comment

  (def token "")
  (util/fetch-input {:token token :year 2021 :day 1})

  )
