(ns ch-9-data-and-code.core
  ;; fine grained var mapping:
  ;; :exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require. 
  (:use midje.sweet))
;; When using `-` in namespaces, the source filename must use `_` instead of `-`.

;; 2 level mapping:
;; symbol -> namespace
;;   containing mappings of
;;     symbols -> vars

;; Creating namespaces:

; (ns name) ;; automatically adds java.lang and clojure.core. Intended for code, not repl.

; (in-ns 'name) ;; imports java.lang, not mappings for functions or macros in clojure.core.
; (clojure.core/refer 'clojure.core) ;; can load clojure.core.
(def my-space (create-ns 'my-namespace))
((ns-map my-space) 'String) ;; check that the symbol 'String is bound to java.lang.String

; (find-ns 'my-namespace)
(intern my-space 'x 9) ;; finds or creats a var
(intern my-space 'reduce clojure.core/reduce)
(intern my-space '+ clojure.core/+)

(in-ns 'my-namespace)
(reduce + [1 2 3 4 5])

(in-ns 'user)

(ns-unmap 'my-namespace 'reduce)
(get (ns-map 'my-namespace) 'reduce)

(remove-ns 'my-namespace)

; (pprint (all-ns))

;; zoology minor chem
;; grad u of w on masters in radiological science
;; 
;;


(ns hider.ns)

(defn ^{:private true} answer [] 42)
;; ^{:private true} is also used for def and defmacro, they don't have private- versions.

(ns seeker.ns
  (:refer hider.ns))

; (answer)

;; It's common to have a src/name/impl/impl.clj that's used by src/name/<whatever lib>.clj
;; to hide implementation details.

; (ns joy.ns-ex
; 	(:refer-clojure :exclude [defstruct])
; 	(:use (clojure set xml)) ;; don't use this in production, only import & export the needed elements.
; 	(:use [clojure.test :only (are is)])
; 	(:require (clojure [zip :as z]))
; 	(:import (java.util Date)
; 					 (java.io File)))

;; get back to the expected namespace.
(ns ch-9-data-and-code.core
  (:use midje.sweet))

;; Records

(defrecord TreeNode [val l r])
;; N.B.: defrecord and deftype classes must be specifically imported:
;; (ns ... (:import ch-9-data-and-code.core.TreeNode))
;; Although the ->RecordName is imported by default!

;; Records can't be used as functions -- keywords can lookup record vals though. Destructuring works too!
;; Records are never equal to maps with the same k/v mappings.

;; You can just build a ton of code using hash maps and make minimal changes to use records when needed for performance.

;; from ch-6-lazy-and-set
(defn xconj [t v]
  (cond
    (nil? t)       (->TreeNode v nil nil)
    (< v (:val t)) (->TreeNode (:val t) (xconj (:l t) v) (:r t))
    :else          (->TreeNode (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))

(xseq sample-tree)


(fact
  "Records"
  (->TreeNode 5 nil nil) => #ch_9_data_and_code.core.TreeNode{:val 5, :l nil, :r nil}
  (->TreeNode 5 nil nil) =not=> #:.TreeNode{:val 5, :l nil, :r nil} ;; is can do this?

  ;; Note that this isn't a TreeNode anymore:
  (dissoc (->TreeNode 5 nil nil) :l) => {:val 5 :r nil}
  ;; it's a map
  (assoc (->TreeNode 5 nil nil) :z 20) => #ch_9_data_and_code.core.TreeNode{:val 5, :l nil, :r nil, :z 20} 
  ;; is still a TreeNode though.
  )

;; They're also awesome with protocols!


