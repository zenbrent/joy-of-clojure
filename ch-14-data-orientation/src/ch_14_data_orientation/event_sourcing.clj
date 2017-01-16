(ns ch-14-data-orientation.event-sourcing
  (:require [ch-14-data-orientation.core :refer [rand-map]]))

;; Event sourcing
;; A system that defines system state solely in terms of system events (Fowler 2005)

;; 14.3.3

(defn valid? [event]
  (boolean (:result event)))

(valid? {}) ;=> false
(valid? {:result 123}) ;=> true

(defn effect [{:keys [ab h] :or {ab 0, h 0}}
              event]
  (let [ab (inc ab)
        h (if (= :hit (:result event))
            (inc h)
            h)
        avg (double (/ h ab))]
    {:ab ab :h h :avg avg}))

(effect {} {:result :hit})
(effect {:ab 599 :h 180}
        {:result :out})

(defn apply-effect [state event]
  (if (valid? event)
    (effect state event)
    state))

(apply-effect {:ab 600 :h 180 :avg 0.3}
              {:result :hit})

(def effect-all #(reduce effect %1 %2))

(effect-all {:ab 0, :h 0}
            [{:result :hit}
             {:result :out}
             {:result :hit}
             {:result :out}])

(def events (repeatedly 100
              (fn []
                (ch-14-data-orientation.core/rand-map 1
                  #(-> :result)
                  #(if (< (rand-int 10) 3)
                     :hit
                     :out)))))

(effect-all {:ab 0, :h 0} events)
;; to rewind:
(effect-all {:ab 0, :h 0} (take 50 events))

(def fx-timeline
  "see the entire history of changes!"
  #(reductions apply-effect %1 %2))

(fx-timeline {} (take 50 events))

