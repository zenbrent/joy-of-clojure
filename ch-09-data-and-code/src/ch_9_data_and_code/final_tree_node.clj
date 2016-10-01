(ns ch-9-data-and-code.final-tree-node
  (:use midje.sweet))

;; protocol methods dispatch on the type of the first arg.
(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(deftype TreeNode [val l r]
  FIXO
  (fixo-push [_ v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [_]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [_]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r))
  
  clojure.lang.IPersistentStack
  (cons [this v] (fixo-push this v))
  (peek [this] (fixo-peek this))
  (pop [this] (fixo-pop this))

  clojure.lang.Seqable
  (seq [t]
    (concat (seq l) [val] (seq r))))

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil))) 

(fact
  "FIXO"
  (let [sample-tree (into (TreeNode. 3 nil nil) [5 2 4 6])]
    (seq sample-tree) => [2 3 4 5 6]))

;; deftype lets you create volatile & mutable fields. but it's almost never the right solution.



