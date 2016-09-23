(ns monsters.core)

;; Human vs monster
;;   People
;;     Human
;;       Name
;;       Has Killed?
;;   Monsters
;;     Chupacabra
;;       Eats Goats?

;; Clojure is a design language where the conceptual model is also Clojure.

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



