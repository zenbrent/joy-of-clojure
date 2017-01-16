(ns ch-14-data-orientation.sim-test
  (:require [ch-14-data-orientation.core :refer [rand-map]]
            [ch-14-data-orientation.event-sourcing :as es]
            [clojure.set :as sql]))

(def PLAYERS #{{:player "Alex" :ability 42}
               {:player "Blue" :ability 31}
               {:player "Brent" :ability 27}
               {:player "Jon" :ability 29}})

(defn lookup [db name]
  (first (sql/select
           #(= name (:player %)) ;; pretend it's a sql source
           db)))

(lookup PLAYERS "Brent") 

(defn update-stats [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db
          (merge player (es/effect player event))))) ;; play the event onto the player, not a game.

;; 14.9 transactionally applying result events to a data store.
(defn commit-event [db event]
  (dosync (alter db update-stats event)))

(def pl (ref PLAYERS))
(commit-event pl {:player "Brent", :result :hit})

(defn rand-event [{ability :ability}]
  (let [able (numerator ability)
        max (denominator ability)]
    (rand-map 1
              #(-> :result)
              #(if (< (rand-int max) able)
                 :hit
                 :out)))) 

(defn rand-events [total player]
  (take total
        (repeatedly #(assoc (rand-event player)
                            :player
                            ))
        )
  )

