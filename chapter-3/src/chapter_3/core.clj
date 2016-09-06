(ns chapter-3.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; learning the repl
;; (find-doc string)
;; e.g. (find-doc "xor") ;=> clojure.core/bit-xor ...docs...
;; also javadoc:
;; (javadoc symbol)
;; (.printStackTrace *e) ;; *e is a special repl var for the last thrown error!

(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y (rem (bit-xor x y) 256)]))


(defn reload-ns
  "Reload the current ns. Only for use in the repl."
  []
  (use (symbol (str *ns*)) :reload))

(defn find-methods
  "Find methods on a Java class.
   e.g. (find-methods java.awt.Frame #\"Vis\")"
  [class regexp]
  (for [meth (.getMethods class)
        :let [name (.getName meth)]
        :when (re-find regexp name)]
    name))

;; p61

; (def frame (java.awt.Frame.))

; (.setVisible frame true)
; (.setSize frame (java.awt.Dimension. 500 500))
; ; ; Any reason that is better than this?
; ; ; (.setSize frame 500 400)

; (def gfx (.getGraphics frame))

; (.fillRect gfx 100 100 50 75)
; (.setColor gfx (java.awt.Color. 255 128 0))
; (.fillRect gfx 100 150 75 50)

(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(def ^:dynamic *scale* 1)

(defn draw-values [f g xs ys]
  (let [xs*scale (* *scale* xs)
        ys*scale (* *scale* ys)]
    (clear g xs*scale ys*scale)
    (.setSize frame (java.awt.Dimension. xs*scale ys*scale))
    (doseq [[x y v] (f-values f xs ys)]
      (.setColor g (java.awt.Color. v v v))
      (.fillRect g (* *scale* x) (* *scale* y) *scale* *scale*))))


(defn clear 
  ([g]
   (clear g 200))
  ([g x]
   (clear g x x))
  ([g x y]
   (.clearRect g 0 0 x y)))
