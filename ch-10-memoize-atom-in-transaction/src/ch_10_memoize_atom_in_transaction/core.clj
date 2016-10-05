(ns ch-10-memoize-atom-in-transaction.core
  (:use [clojure.pprint :only [pprint]]))

;; Memoization with atoms
;; but IRL just use (memoize ...) or core.memoize.

(defn manipulable-memoize [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 60) x))

; (time [(slowly 9) (slowly 9)])

(def sometimes-slowly (manipulable-memoize slowly))

(time [(sometimes-slowly 99) (sometimes-slowly 99)])

(meta sometimes-slowly)

(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(9))) ;; Could use reset! instead of swap!, but it's usu good to
                             ;; set reference vals using application of a fn instead of
                             ;; directly setting a value.



