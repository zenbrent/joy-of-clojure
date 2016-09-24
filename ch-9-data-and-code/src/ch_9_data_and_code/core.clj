(ns ch-9-data-and-code.core
  ;; fine grained var mapping:
  ;; :exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require. 
  )
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



