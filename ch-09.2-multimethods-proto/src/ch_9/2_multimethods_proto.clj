(ns ch-9.2-multimethods-proto
  (:refer-clojure :exclude [get])
  (:use midje.sweet))

;; Yegge's "Universal Design Pattern", re: Hofstadter
;; aka prototypical inheritance.

(defn beget [this proto]
  (assoc this ::prototype proto))

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)] ;; find returns [:key val] or nil. Good to make sure the val isn't just nil, but that hte key exists.
      v
      (recur (::prototype m) k))))

(def put assoc) ;; asymmetric to get. put only operates on the supplied level of a prototype chain.

;; Can think about these as being examples of facts (fact ...)
;; or facts themselves (facts ...)
;; they are  the same, you can treat it as a misspelling of test if you want..
(fact
  "`beget` extends prototypes"
  (beget {:sub 0} {:super 1}) => {::prototype {:super 1}, :sub 0}
  (get (beget {:sub 0} {:super 1})
       :super) => 1
  (put (beget {:sub 0} {:super 1})
       :super 2) => {::prototype {:super 1}, :super 2 :sub 0})

(fact
  "The dog was provoked. Proof: cats are evil."
  (let [cat {:likes-dogs true, :ocd-bathing true}
        morris (beget {:likes-9lives true} cat)
        post-traumatic-morris (beget {:likes-dogs nil} morris)]
    (get cat :likes-dogs) => true
    (get morris :likes-dogs) => true
    (get post-traumatic-morris :likes-dogs) => nil
    (get post-traumatic-morris :likes-9lives) => true))


;; To create behaviors that work against different "types" of a prototype map:
(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m :llvm-compiler))

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))
(defmethod home ::bsd [m] "/home")
(prefer-method home ::unix ::bsd)
(remove-method home ::bsd) ;; <= lol who uses bsd

;; Arbitrary dispatch!
(defmulti compile-cmd (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"] [m]
 (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m]
 (str "Unsure where to locate " (get m :c-compiler)))

; (defmethod compile-cmd [::unix "clang"] )

;; http://stackoverflow.com/questions/3012088/when-and-how-should-independent-hierarchies-be-used-in-clojure
;; independent hierarchies should be used when more than one hierarchy is needed, or when the hierarchy needs
;; to be modified extensively during run time. Also, if we want a hierarchy for naked keywords, an independent
;; hierarchy is required.

(fact
  "demoing multimethods"
  (let [clone (partial beget {})
        unix {:os ::unix, :c-compiler "cc", :home "/home", :dev "/dev"}
        osx (-> (clone unix)
                (put :os ::osx)
                (put :llvm-compiler "clang")
                (put :home "/Users"))]
    (derive ::osx ::unix) ;; this manipulates a GLOBAL structure.
    (derive ::osx ::bsd)

    (compiler unix) => "cc"
    (compiler osx) => "clang"

    (home unix) => "/home"
    (home osx) => "/Users" ;; this errors if home doesn't have a prefer-method list

    (mapv parents [::osx ::unix]) => [#{::unix ::bsd} nil]
    (mapv ancestors [::osx ::unix]) => [#{::unix ::bsd} nil] 
    (mapv descendants [::osx ::unix]) => [nil #{::osx}]
    (isa? ::osx ::unix) => true
    (isa? ::unix ::osx) => false
    (compile-cmd osx) => "Unsure where to locate cc"))

;; Juxt is AWESOME:
(def each-math (juxt + * - /))
(map each-math (range 5) (range 5 10))

((juxt take drop) 3 (range 9))
;; Awesome!!!

