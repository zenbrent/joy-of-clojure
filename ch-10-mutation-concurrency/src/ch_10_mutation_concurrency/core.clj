(ns ch-10-mutation-concurrency.core
  (:import java.util.concurrent.Executors)
  (:use [ch-10-mutation-concurrency.dothreads :refer [dothreads!]]
        [midje.sweet]
        [clojure.pprint :refer [pprint]]
        [clojure.string :refer [join]]))

;; These are used to define identity, and determines change semantics:

;;;; Refs
;; Coordinated - multiple refs can be changed without race conditions.
;; Retriable -- changes are speculative and may have to be retried.
;;;; Agents
;; Async
;;;; Atoms
;; Retriable
;;;; Vars
;; Thread-local

;; All but vars are  considered shared references and allow changes to be seen
;; across threads of execution. Even though their use cases overlap, each
;; has an ideal use.

;; Concurrency is running multiple things at once
;; parallelism is partitioning work until multiple pieces that can run concurrently, typically towards building an aggregate.

;; "A faster program that doesn’t work right is useless." -- Peyton-Jones in Beautiful Concurrency

;; Time, state, identity:
;; relative moments that events occur
;; a snapshot of an entity at a moment in time
;; the logical entity identified by a common stream of states occuring over time.

;; Transaction is demarked by (dosync ...)
;; Changes in a dosync all succeed or fail together.

;; Deref all types with # or (deref ...)
;; They all mutate with different functions, but those all have the same structure.

;; 10.1.1 Making a mutable game board

(def initial-board
  [[:- :k :-]
   [:- :- :-] 
   [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %] (f s)))
            board)))

(defn board-print [board]
  (println (join "\n" 
                 (map #(->> %
                            (map name)
                            (join " "))
                      (board-map deref board)))))

(defn reset-board!
  "Resets the board state. Generally these types of functions are a bad idea."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]])) ;; describes the order of the moves
  (def num-moves (ref 0)))

(declare neighbors)

(def king-moves
  "Legal moves for the king"
  (partial neighbors
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move?
  "A move is legal when an enemy isn't there."
  [to enemy-sq]
  (when (not= to enemy-sq)
    to)) ;; nil if occupied

(defn choose-move
  "Randomly choose a legal move"
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

;; from 5.1
(def neighbors4
  [[-1 0] [1 0] [0 -1] [0 1]])

(defn neighbors
  "Returns a vector of [x y] pairs for neighboring cells on a grid."
  ([size yx]
   (neighbors neighbors4
              size
              yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

; (pprint (do (reset-board!)
;             (take 5 (repeatedly #(choose-move @to-move)))))

(defn place [from to] to)

;; Commute allows for more concurrency than alter or ref-set
;; but transaction is ran at least twice
;; and must be commutative, or at least last-one-in-wins.
(defn move-piece [[piece dest] [[_ src] _]]
  (commute (get-in board dest) place piece) ;; get-in is awesome
  (commute (get-in board src) place :-)
  (commute num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (dosync
    (let [move (choose-move @to-move)]
      (move-piece move @to-move)
      (update-to-move move))))

; (reset-board!)
; (doseq [_ (range 10000)] (make-move))
; (board-print board)

(reset-board!)
(dothreads! make-move :threads 100 :times 1000)
(board-print board)

;; Don't do this usually:
(dosync (ref-set to-move '[[:K [3 1]] [:k [1 1]]]))

;; Embedded transaction: clojure absorbs nested transactions into the parent,
;; so if one fails, they're all retried.

;; STM provides:
;; ACI -- atomicity, consistency, isolation, w/o manual locking.
;; in memory, so durability isn't relevant.
;; But:
;; Write skew -- in mvcc (multiversion concurrency control) happens when on transaction
;; uses a reference value to control it's behavoir, but doesn't write to it, and another
;; transaction writes to that value. Clojure can prevent this with `ensure`.
;; Live lock -- a set of transaction(s) that repeatedly restart each other. Clojure does
;; fight this.

;; In a transaction, don't:
;; IO -- because retries. Use io! macro when doing io to ensure it isn't used in a transaction.
;; e.g. (io! (.println System/out "sups yo!"))
;; (dosync (io! (.println System/out "sups yo!"))) ;; throws error
;; Mutate things -- b/c it's usu. not idempotent.
;; Large transactions -- do as little work as possible in them.


;; Snapshot isolation: in a transaction, all ref values used will be from the same moment in time.
;; Algorithms should be designed so they only care that values haven't changed before a commit.
;; (unless it's commutative)
;; Use (ensure ref) when referencing a value that isn't written to, which gurantees that a read-only
;; ref isn't written to in a transaction.

;; 10.2.4 Refs under stress

;; Avoid having both short and long running transactions interacting with a single ref.

(defn stress-ref [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
        (swap! slow-tries inc)
        (Thread/sleep 200)
        @r)
      (println (format "r is: %s, history: %d, after: %d tries"
                       @r (.getHistoryCount r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

; (stress-ref (ref 0))
;; r is: 500, history: 10, after: 31 tries
;; r is: 500, history: 10, after: 30 tries

;; If you REALLY have to mix long and short transactions:
; (stress-ref (ref 0 :max-history 30))
; r is: 300, history: 17, after: 18 tries
; (stress-ref (ref 0 :max-history 50))
; 298, history: 17, after: 18 tries
; (stress-ref (ref 0 :max-history 100))
; r is: 279, history: 16, after: 17 tries

;; Since it looks like it needs about 20 items in the history:
; (stress-ref (ref 0 :min-history 15 :max-history 30))
; r is: 33, history: 16, after: 2 tries

;; Agents!

;; Each agent has a queue of actions to be performed on it's value.
;; Each action produces a new value for the agent to pass to subsequent values.
;; queue actions w/ send or send-off
;; send & send-off are not considered side-effects in context of dosync

(def joy (agent []))

; (send joy conj "First edition")

(defn slow-conj [coll item]
  (Thread/sleep 1000)
  (conj coll item))

(send joy slow-conj "First edition")
@joy

;; await lets you block a thread until an agent has processed a message,
;; but it won't work in agent threads or transactions to prevent deadlocks.


;; Controlling IO

(def log-agent (agent 0))

;; All actions take the curr state as their 1st arg, then other args passed in.
(defn do-log [msg-id message]
  (println msg-id ":" message)
  (inc msg-id))

(defn do-step [channel message]
  (Thread/sleep 1)
  (send-off log-agent do-log (str channel message)))

(defn three-step [channel]
  (do-step channel " ready to begin (step 0)")
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting going now (step 2)")
  (do-step channel " done! (step 3)"))

(defn all-together-now []
  (dothreads! #(three-step "alpha"))
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "omega")))

; (do-step "important: " "this must go out!")
; (await log-agent)

; (send log-agent (fn [_] 1000)) ;; set a value
; (do-step "epsilon " "near miss!")

;; send-off -- a single action queue is used
;; send -- a single action queue + agent waits in a thread pool's queue.
;;         The thread pool size is based on the # of threads the JVM has, so
;;         don't use it with anything that takes a long time or it'll block
;;         everything else.

(defn exercise-agents [send-fn thread-count]
  (let [agents (map #(agent %) (range thread-count))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))

; (time (exercise-agents send-off 10))
; (time (exercise-agents send 10))
; (time (exercise-agents send-off 11))
; (time (exercise-agents send 11))
; (time (exercise-agents send-off 20))
; (time (exercise-agents send 20))


;; :error mode -- default
; (agent-error log-agent) ;=> nil
; (send log-agent (fn [] 1234)) ;; agent errors and is stopped, sending another action will make throw.
; (println @log-agent) ;=> 0
; (send log-agent (fn [_] 1234)) ;=> throws error
; (agent-error log-agent) ;=> prints error

; (restart-agent log-agent 2500 :clear-actions true) ;; :clear-actions removes pending items from the queue
; (restart-agent log-agent 2500)                     ;; otherwise they'll all run

; (println @log-agent) ;=> 2500
; (restart-agent log-agent 2500) ;=> error

;; :continue mode
(defn handle-log-error [the-agent the-err]
  (println "An action sent to the log-agent threw " the-err))

(set-error-handler! log-agent handle-log-error)
(set-error-mode! log-agent :continue)

; (send log-agent (fn [] 1234)) ;=> logs error
; (send log-agent (fn [_] 1000))
; (println @log-agent) ;=> 1000
; (agent-error log-agent) ;=> nil
; (send-off log-agent do-log "Stayin' alive, stayin' alive...")

;; fail mode supports handlers, but they can't restart the agent, so not usu. useful.

;; Don't use agents as a thread pool. Use a thread pool.


;; 10.4 Atoms
;; Synchronous like refs
;; Uncoordinated like agents
;; Useful for CAS spinning operations (keep checking for a value in a loop)
;; i.e. atomically compute a value given an existing value and swap in the new one.
;; Updates are local to calling thread.
;; DOES NOT OCCUR IN STM -- not coordinated with other reference types -- not rolled back
;;                          if a transaction fails. Only use in a transaction if idempotent!

;; Update the mutable reference with:
;; swap!
;; compare-and-set!
;; reset!

; (def ^:dynamic *time* (atom 0))
; (defn tick [] (swap! *time* inc))
; (dothreads! tick :threads 100 :times 100)

;; 10.6 Vars
;; thread local
;; can be named and interned in a ns -- most refs are assigned to something with a name and you deref them,
;;                                      referencing a var by name gives you it's value. Use var to get the
;;                                      reference obj.

; *read-eval* => true ;; root binding for *read-eval*
; (var *read-eval*) => #'*read-eval*

(defn print-read-eval []
  (println "*read-eval* is currently " *read-eval*))

(defn binding-play []
  (print-read-eval)
  (binding [*read-eval* false]
    (print-read-eval))
  (print-read-eval))

;; Creating vars:
; defn -- puts a fn in a var
; defmacro
; defonce -- sets the value of an unbound var, or doesn't evalue argument.
; defmulti -- multimethod in a var

;; vars can exist (or not) in one of 4 states:
; (def x) ;; unbound
; (def x 5) ;; bound, not thread-bound
; (binding [x 7] ...) ;; thread-bound
; (with-local-vars [x 9] ...) ;; thread-bound, but (resolve #'x)s to nil

;; To test each, use:
; (resolve 'x)
; (bound? #'x)
; (thread-bound? #'x)

;; with-local-vars creates a local var
;; since it's bound to a local, it's not implicitly looked up by symbolic name. use deref or var-get.
;; This is very rarely used.

(def x 42.5)
{:outer-var-value x
 :with-locals (with-local-vars [x 9]
                {:local-var x
                 :local-var-value (var-get x)})}

;; vars have dynamic scope:
(with-precision 4 ;; uses `binding` macro
  (/ 1M 3))

;; throws an error, because BigDecimal won't round unless you tell it that's ok:
; (/ 1M 3) 

;; This errors too:
; (with-precision 4
;   (map (fn [x] (/ x 3)) (range 1M 4M)))
;; binding uses dynamic scope, not lexical
;; also issue with agents, futures, etc.

;; This works, but it's not lazy anymore:
(with-precision 4
  (doall (map (fn [x] (/ x 3)) (range 1M 4M))))

;; recreates the dynamic scope for each call:
(with-precision 4
  (map (bound-fn [x] (/ x 3)) (range 1M 4M)))

;; try/catch --
;; e.g. with-open uses try/finally to clean up automatically after execution leaves it's dynamic scope.
;; bound-fn doesn't help, you have to write to the file before leaving it's dynamic scope.

;; summary!
;; Clojure doesn't foster concurrency, but provides tools for sane management of state.
;; That makes sane-r concurrency.

