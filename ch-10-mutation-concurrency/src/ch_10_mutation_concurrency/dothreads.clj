(ns ch-10-mutation-concurrency.dothreads
  (:import java.util.concurrent.Executors)
  (:use [midje.sweet]
        [clojure.pprint :refer [pprint]]))

(def thread-pool
  (Executors/newFixedThreadPool
    (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(fact
  "`dothreads!` does work on multiple threads."
  (dothreads! #(.print System/out "Hi ") :threads 2 :times 2))

