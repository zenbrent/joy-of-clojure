(ns ch-11-parallelism.core
  (:use [ch-11-parallelism.dothreads :refer [dothreads!]])
  (:require (clojure [xml :as xml]))
  (:require (clojure [zip :as zip]))
  (:import (java.util.regex Pattern)))

;; Concurrency is about the design of a system
;; Parallelism is about the execution model.

;; Futures represent work that is to be done.
;; partition typically sequential operations into discrete parts that can be
;; asynchronously processed across several threads.

; (time (let [x (future (do (Thread/sleep 50) (+ 41 2)))]
;         [@x @x]))

; (let [x (future (do (Thread/sleep 50) (+ 41 2)))]
;         (time [x])
;         (time [@x]) ;; it's only evaluated when you dereference it.
;         (time [@x @x]))

(defn feed->zipper [uri-str]
  (->> (xml/parse uri-str)
       zip/xml-zip))

(defn normalize [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag)))) ;; grab entries

(defn title [entry]
  (some->> entry
           :content ;; get the feed content
           (some #(when (= :title (:tag %)) %)) ;; get the title
           :content
           first)) ;; assume there's only 1

(defn count-text-task [extractor txt feed]
  (let [items (feed-children feed)
        re (Pattern/compile (str "(?i)" txt))]
    (->> items
         (map extractor) ;; get children text
         (mapcat #(re-seq re %)) ;; map against each
         count)))

; (count-text-task
;   title
;   "Erlang"
;   "http://feeds.feedburner.com/ElixirLang")

; (count-text-task
;   title
;   "Elixir"
;   "http://feeds.feedburner.com/ElixirLang")

(def feeds #{"http://feeds.feedburner.com/ElixirLang"
             "http://blog.fogus.me/feed/"})

; (let [results (for [feed feeds]
;                 (future
;                   (count-text-task title "Elixir" feed)))]
;   (pprint results)
;   (reduce + (map deref results)))

;; use that patter to build a seq of futures
;; or

(defmacro as-futures [[a args] & body]
  (let [parts (partition-by #{'=>} body) ;; parallel actions are seperated from the summation by =>
        [acts _ [res]] (partition-by #{:as} (first parts)) ;; name the results
        [_ _ task] parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))

(defn occurences [extractor tag & feeds]
  (as-futures [feed feeds]
    (count-text-task extractor tag feed)
    :as results
   =>
    (reduce + (map deref results))))

; (println
;   (occurences title "Released"
;               "http://blog.fogus.me/feed/"
;               "http://feeds.feedburner.com/ElixirLang"
;               "http://www.ruby-lang.org/en/feeds/news.rss"))

;; If a computation freezes, then the deref call will, to. Use:
;; future-done?
;; future-cancel
;; future-cancelled?
;; to "skip, retry, or eliminate ornery feeds"

;; 11.2 Promises </3

;; "Futures encapsulate an arbitrary expression that caches its value in the future on completion
;;  Promises are placeholders for values whos construction is fulfilled on another thread via `deliver`."

(def x (promise))
(def y (promise))
(def z (promise))

; (dothreads! #(deliver z (+ @x @y)))

; (dothreads!
;   #(do (Thread/sleep 10) (deliver x 52)))

; (dothreads!
;   #(do (Thread/sleep 20) (deliver y 86)))

; (time @z)

;; Promises must write once and only once!

(defmacro with-promises [[n tasks _ as] & body]
  (when as
    `(let [tasks# ~tasks
           n# (count tasks#)
           promises# (take n# (repeatedly promise))]
       (dotimes [i# n#]
         (dothreads!
           (fn []
             (deliver (nth promises# i#)
                      ((nth tasks# i#))))))
       (let [~n tasks#
             ~as promises#]
         ~@body))))

(defn feed-items [k feed]
  (k
   (for [item (filter (comp #{:entry :item} :tag)
                      (feed-children feed))]
     (-> item :content first :content))))

(feed-items
  count
  "http://blog.fogus.me/feed/")

(defn cps->fn [f k]
  (fn [& args]
    (let [p (promise)]
      (apply f (fn [x] (deliver p (k x))) args)
      @p)))

(def count-items (cps->fn feed-items count))

(count-items "http://blog.fogus.me/feed/")

;; Only a single thread can deliver on a promise, so only that thread can cause a deadlock.

(def kant (promise))
(def hume (promise))

(dothreads!
  #(do (println "Kant has" @kant) (deliver hume :thinking)))

(dothreads!
  #(do (println "Hume has" @kant) (deliver kant :fork)))

;; @kant or @hume will cause a deadlock.

