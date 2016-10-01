(ns ch-9-chess-builder.core
  (:refer-clojure :exclude [get])
  (:use [midje.sweet]
        [clojure.pprint :refer [pprint]]))

(def example-move {:from :e7
                   :to :e8
                   :castle? false
                   :promotion \Q})

(defn build-move-v1 [& pieces]
  (apply hash-map pieces))

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (name (:from this))
         " to " (name (:to this))
         (if (:castle? this) " castle"
           (if-let [p (:promotion this)]
             (str " promote to " p))))))

(defn build-move-v2 [& pieces]
  (map->Move (apply hash-map pieces)))

(defn build-move-final [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (->Move from to castle? promotion))

(fact "something"
      (build-move-v1 :from :e7
                     :to :e8
                     :castle? false
                     :promotion \Q) => example-move
      (build-move-v2 :from :e7
                     :to :e8
                     :castle? false
                     :promotion \Q) => example-move
      (str (build-move-final :from :e2 :to :e4)) => "Move e2 to e4")

