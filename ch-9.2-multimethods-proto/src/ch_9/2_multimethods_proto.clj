(ns ch-9.2-multimethods-proto
  (:refer-clojure :exclude [get])
  (:use midje.sweet))

;; Yegge's "Universal Design Pattern", re: Hofstadter
;; aka prototypical inheritance.

(defn beget [this proto]
  (assoc this ::prototype proto))

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)] ;; find returns [:key val] or nil. Good to make sure the val isn't just nil, but that hte key exists.
      v
      (recur (::prototype m) k))))

(def put assoc) ;; asymmetric to get. put only operates on the supplied level of a prototype chain.

;; Can think about these as being examples of facts (fact ...)
;; or facts themselves (facts ...)
;; they are  the same, you can treat it as a misspelling of test if you want..
(fact
  "`beget` extends prototypes"
  (beget {:sub 0} {:super 1}) => {::prototype {:super 1}, :sub 0}
  (get (beget {:sub 0} {:super 1})
       :super) => 1
  (put (beget {:sub 0} {:super 1})
       :super 2) => {::prototype {:super 1}, :super 2 :sub 0})

(fact
  "The dog was provoked. Proof: cats are evil."
  (let [cat {:likes-dogs true, :ocd-bathing true}
        morris (beget {:likes-9lives true} cat)
        post-traumatic-morris (beget {:likes-dogs nil} morris)]
    (get cat :likes-dogs) => true
    (get morris :likes-dogs) => true
    (get post-traumatic-morris :likes-dogs) => nil
    (get post-traumatic-morris :likes-9lives) => true))


