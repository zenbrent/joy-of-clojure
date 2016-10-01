(ns ch-10-mutation-concurrency.core
  (:refer-clojure :exclude [get])
  (:use [midje.sweet]
        [clojure.pprint :refer [pprint]]))

