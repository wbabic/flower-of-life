(ns edn-play.applicative)


(defprotocol Functor
  (fmap [functor f] "Maps fn over the functor f"))

(defrecord List [wrapped]
  Functor
  (fmap [functor f]
    (List. (map f (:wrapped functor)))))


;; http://www.leonardoborges.com/writings/2012/12/02/monads-in-small-bites-part-ii-applicative-functors/


;; it dispatches on the record type since we could have
;; implementations of pure for List, Maybe, Either etc...
(defmulti pure (fn [f _] f))
(defmethod pure List [_ v]
  "Wraps value v in a list"
  (List. [v]))

;; it dispatches on the class of the Functor instance passed in the
;; 1st argument
(defmulti <*> (fn [fs _] (class fs)))
(defmethod <*> List [fs xs]
  "Unwraps the functions in fs, applies them to the functors in xs
wrapping the result at the end"
  (List. (for [f (:wrapped fs)
               x (:wrapped xs)]
           (f x))))
(comment
  (def fs (List. [#(* 2 %) #(+ 10 %)]))
  (def xs (List. [1 2 3]))

  (<*> fs xs)

  (def g (pure List #(* 50 %)))
  (<*> g xs)

  (def f #(+ 2 %))
  (def v (List. [10]))

  ;; identity
  (<*> (pure List f) v)
  (fmap v f)
  
  )

(comment
  (def u (List. [#(* 2 %)]))
  (def v (List. [#(+ 10 %)]))
  (def w (List. [1 2 3]))

  (-> (pure List (fn [x] (partial comp x)))
      (<*> u)
      (<*> v)
      (<*> w))

  (<*> u (<*> v w)))

(comment
  (def f #(* 2 %))
  (def x 10)

  (-> (pure List f)
      (<*> (pure List x)))

  (pure List (f x)))

(comment
  (def u (pure List #(+ 10 %)))
  (def y 50)

  (<*> u (pure List y))

  (def dollar-y #(% y))

  (<*> (pure List dollar-y) u)
  )


;; monoids
(def  mempty (+)) ;; 0
(def  mappend +)
(defn mconcat [ms]
    (reduce mappend mempty ms))

(comment
  (mappend 3 4)
  (mconcat [2 3 4])

  (+)
  (+ 3 4)
  (reduce + [2 3 4])
  
  )

(defn plus-monoid
    ([]
        0)
    ([a b]
        (+ a b)))

(comment
  (plus-monoid)
  ;; 0 - same as mempty

  (plus-monoid 3 4)
  ;; 7 - same as mappend

  (reduce plus-monoid [2 3 4])
  ;; 9 - when working with monoids, reduce
  ;; is the same as mconcat
  )

(defn mult-monoid
  ([] 1)
  ([a b] (* a b)))

(comment
  (mult-monoid)
  (mult-monoid 2 3)
  (reduce mult-monoid [2 3 4])
  )

(defn list-monoid
  ([] '())
  ([a b] (concat a b)))

(comment
  (list-monoid)
  (list-monoid [1 2 3] [4 5 6])
  (reduce list-monoid [[1 2 3] [4 5 6] [7 8 9]])
  )

(comment
  ;; monoid laws
  ;; identity
  (def mempty (mult-monoid))
  (mult-monoid mempty 3)
  (mult-monoid 3 mempty)

  ;; associativity
  (mult-monoid 2 (mult-monoid 3 4))
  (mult-monoid (mult-monoid 2 3) 4)

  (def x [40])
  (def y [10 25])
  (def z [50])
  (list-monoid x (list-monoid y z))
  (list-monoid (list-monoid x y) z)
  )


(def maybe-monad
  {:return (fn [v] v)
   :bind (fn [mv f]
           (if mv
             (f v)
             nil))})

(def list-monad {
    :return (fn [v] [v])
    :bind (fn [mv f]
            (if (seq mv)
              (apply concat (map f mv))
              []))})

