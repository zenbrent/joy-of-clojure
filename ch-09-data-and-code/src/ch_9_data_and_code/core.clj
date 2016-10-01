(ns ch-9-data-and-code.core
  ;; fine grained var mapping:
  ;; :exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require. 
  (:use 
    [ch-9-data-and-code.final-tree-node :only []]
    [midje.sweet]))
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

;; They're also awesome with protocols! More on that later.

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

;; To define the methods polymorphically (if that's important) use named recursion:
;; Also, implementing it inline makes dramatically faster code.
;; The record's protocol cannot be redefined later, though.
;; Inline definition is the only way to use implement java interfaces and extend java.lang.Object
;; Interfaces can use primitives, not just boxed values, so inline implementations can also
;;;; support primitives. GTK for interop and can provide performance parity with Java code.
;; Also! When using recur on an inline method, recur doesn't pass the first arg.
(defrecord TreeNode [val l r]
  FIXO
  (fixo-push [t v]
    (if (< v val)
      (->TreeNode val (fixo-push l v) r) ;; the record's values are available as locals.
      (->TreeNode val l (fixo-push r v))))
  (fixo-peek [t]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [t]
    (if l
      (->TreeNode val (fixo-pop l) r)
      r)))

(extend-type clojure.lang.IPersistentVector
	FIXO
	(fixo-push [vector value]
		(conj vector value))
	(fixo-peek [vector]
		(peek vector))
	(fixo-pop [vector]
		(pop vector)))

; (println (xseq (fixo-push sample-tree 5/2)))

(fixo-push [2 3 4 5 6] 5/2)

(extend-type nil
  FIXO
  (fixo-push [t v]
    (->TreeNode v nil nil)))

(xseq (reduce fixo-push nil [3 5 2 4 6 0]))

(def sample-tree2 (reduce fixo-push (->TreeNode 3 nil nil) [5 2 4 6]))

; (extend-type TreeNode
;   FIXO
;   (fixo-push [node value]
;     (xconj node value))
;   (fixo-peek [node]
;     (if (:l node)
;       (recur (:l node))
;       (:val node)))
; 	(fixo-pop [node]
; 		(if (:l node)
; 		 (->TreeNode (:val node) (fixo-pop (:l node)) (:r node))
; 		 (:r node))))

;; A protocol can be partially implemented, but a type can only have 1 implementation, i.e.
;; (extend-typing TreeNode FIXO (fixo-pop ...)) will remove the fixo-push implementation.

;; How to extend a class then build another class on top of that?

;; 1. Make a function that builds on the protocol's methods:
(defn fixo-into  [c1 c2]
	(reduce fixo-push c1 c2))

(xseq (fixo-into (->TreeNode 5 nil nil) [2 4 6 7]))

;; 2. extend-type

; (def tree-node-fixo
; 	{:fixo-push (fn [node value]
; 									(xconj node value))
; 	 :fixo-peek (fn [node]
; 									(if (:l node)
; 										(recur (:l node))
; 										(:val node)))
; 	 :fixo-pop (fn [node]
; 								 (if (:l node)
; 									 (->TreeNode (:val node) (fixo-pop (:l node)) (:r node))
; 									 (:r node)))})

; (extend TreeNode FIXO tree-node-fixo)

; (xseq (fixo-into (->TreeNode 5 nil nil) [2 4 6 7]))

(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
   (reify FIXO ;; Note: methods always use the protocol's namespace. Collisions avoided.
     (fixo-push [this value]
       (if (< (count vector) limit)
         (fixed-fixo limit (conj vector value))
         this))
     (fixo-peek [_]
       (peek vector))
     (fixo-pop [_]
       (pop vector))))) 

;; Since methods share their protocol's namespace, you can't have multiple protocols have
;; a method with the same name. Since the protocols are controlled by the same person, they
;; can move to another namespace or rename a method.

(deftype InfiniteConstant [i]
  clojure.lang.ISeq
  (seq [this]
    (lazy-seq (cons i (seq this)))))

(take 3 (->InfiniteConstant 5))

;; assoc, disassoc, keyword lookups, etc. don't work on types, unless you implement them.

(:i (->InfiniteConstant 5))

;; but the Java fields are still available:
(.i (->InfiniteConstant 5))


