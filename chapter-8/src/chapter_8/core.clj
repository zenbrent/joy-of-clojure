(ns chapter-8.core
  (:require [clojure.walk :as walk])
  (:import [java.io BufferedReader InputStreamReader]
          [java.net URL])
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



;; Human vs monster
;;   People
;;     Human
;;       Name
;;       Has Killed?
;;   Monsters
;;     Chupacabra
;;       Eats Goats?

;   ;;    
;    ;;    Clojure is a design language where the conceptual model is also Clojure.
;  ;;  ;; 

;; {:tag <node form> ;<-- domain, grouping, etc.
;;  :attrs {}, ;<-- e.g. :name people
;;  :content [<nodes>]} ;<-- e.g. properties

(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str `~name)},
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond ;; This is kind of overkill, but it's a good example of how to make it very extensible.
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                              {:tag :property,
                               :attrs {:name (str (first p))},
                               :contents nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Person "Could be a dog.")
                    (Human (isa Person)
                           "Hola"
                           [name]
                           [has-killed?]))
          (grouping monsters
                    (Chupacabra
                      "A fierce, yet elusive creature"
                      [eats-goats?]))))

; (pprint d)
(:tag d)
(:tag (first (:content d)))
((comp :name :attrs) (first (:content (second (:content d)))))

(#(-> %
      :content
      second
      :content
      first
      :attrs
      :name) d)

; (use '[clojure.xml :as xml])
; (xml/emit d)
; (use '[clojure.data.json :as json])
; (json/write d *out*)

(defmacro resolution [] `y)

(def y 9)
(let [y 109] (resolution))
(macroexpand '(resolution))

;; Anaphora

(defmacro ^:deprecated awhen
  "Don't use this IRL, use if-let or when-let."
  [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(awhen [1 2 3] (it 2))
(awhen 5 (+ 2 it))
(awhen 1 (awhen 2 [it]))

; (doc with-open)

(defn joc-www []
  (-> "https://www.google.com/robots.txt" URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

; (let [stream (joc-www)]
;   (with-open [page stream]
;     (println (.readLine page))
;     (print "The stream will now close... "))
;   (println "but let's read from it anyway.")
;   (println (try (do (.readLine stream) "No exception!")
;                 (catch java.io.IOException e (str "Caught exception: " (.getMessage e))))))

(defmacro with-resource
  "This is generic and ubiquitous enough across lisps to be considered a Lisp design pattern.
  It's a lot like with-open, but delegates the responsibility for releasing the resource to the caller."
  [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally 
         (~close-fn ~(binding 0))))))

; (println
;   (str "Robots.TxT First Line! :)"
;        (let [stream (joc-www)]
;          (with-resource [page stream]
;            #(.close %)
;            (.readLine page)))))

;   ;;     Clojure programmers don't write their apps in
;    ;;    Clojure. They write the language that they
;  ;;  ;;  use to write their apps in Clojure.

(declare collect-bodies)

(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
      (into '[f] args)
      (apply merge
             (for [con (rest c)]
               (cond (= (first con) 'require)
                     (assoc {} :pre (vec (rest con)))
                     (= (first con) 'ensure)
                     (assoc {} :post (vec (rest con)))
                     :else
                     (throw (Exception.
                              (str "Unknown tag "
                                   (first con)))))))
      (list* 'f args))))

(def doubler-contract
  (contract doubler
    [x]
    (require ;; preconditions
             (pos? x))
    (ensure ;; postconditions
            (= (* 2 x) %))
    [x y]
    (require
      (pos? x) 
      (pos? y))
    (ensure
      (= (* 2 (+ x y)) %))))

; (def times2 (partial doubler-contract #(* 2 %)))
; (def times2bad (partial doubler-contract #(* 3 %)))

; ((partial doubler-contract #(* 2 (+ %1 %2))) 2 3)
; ((partial doubler-contract #(+ %1 %1 %2 %2)) 2 3)
; ((partial doubler-contract #(* 3 (+ %1 %2))) 2 3)

;   ;;     But the most important skill that you can learn on your path toward
;    ;;    macro mastery is the ability to recognize when to avoid using them.
;  ;;  ;;  The general answer, of course, is whenever and as often as you can.

