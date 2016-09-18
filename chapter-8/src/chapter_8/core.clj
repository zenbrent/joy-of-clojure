(ns chapter-8.core
  (:require [clojure.walk :as walk])
  (:gen-class))

; (eval (list (symbol "+") 1 2))
;; Eval is bad b/c it's execution bindings are global.

(defn contextual-eval [ctx expr]
  (eval
    ;; `'~symbol gets the value of the built bindings at runtime.
    ;; ` syntax quote
    ;; ~ unquote
    ;; ~@ unquote-slice
    `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
       ~expr))) 

; (let [x 9, y '(- x)]
;   (println `y)
;   (println ``y)
;   (println ``~y)
;   (println ``~~y)
;   (contextual-eval {'x 36} ``~~y))
;; ~ is a way to "jump" out of a syntax-quote.

(comment
  Macro Rules of Thumb
  
  Don't write a macro if a function will do. Reserve macros to provide syntactic abstractions or create binding forms.
  Write an example usage.
  Expand your example usage by hand.
  Use macroexpand, macroexpand-1, and clojure.walk/macroexpand-all to understand ur implementation.
  Experiment at the REPL.
  Break complicated macros into smaller functions whenever possible.)

;; The most ubiquitous use of macros: creating custom control structures.

;; This can reduce superfluous boilerplate code. You could write it inline, but why?
;; do-until is meant for side-effects because it always returns nil.
;; macros starting with do- tend to be like that.
(defmacro do-until [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                     "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(do-until
  (even? 2) (println "Even")
  (odd? 3) (println "Odd")
  (zero? 1) (println "You never see me")
  :lollipop (println "Truthy thing"))

(macroexpand-1 '(do-until true (prn 1) false (prn 2)))

; (require '[clojure.walk :as walk])
(walk/macroexpand-all '(do-until true (prn 1) false (prn 2)))

; Ruby's unless, the opposite of a when:

;; IRL just use when-not LOL
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

; (unless (even? 3) "Now we see it,")
; (unless (even? 2) "Now we see don't!!!")

;; Macros can be used to combine a number of forms and actions into one consistent view.

(defmacro def-watched
  "Define a var with an onchange handler"
  [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#] ;; Clojure attempts to resolve symbols in the current context, resulting in a fully qualified symbo, resulting in a fully qualified symbol. unquoting a quote fixes that.
                  (println old# " -> " new#))))) 

