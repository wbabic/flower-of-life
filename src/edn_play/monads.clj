(ns edn-play.monads)

(defprotocol Functor
  (fmap [functor f] "Maps fn over the functor f"))

(defrecord List [wrapped]
  Functor
  (fmap [functor f]
    (List. (map f (:wrapped functor)))))

(comment
  (def my-list-functor (List. [1 2 3]))
  (fmap my-list-functor #(* 2 %))

  (fmap my-list-functor identity)
  (identity my-list-functor)


  (def f #(+ 10 %))
  (def g #(* 2 %))

  (fmap my-list-functor (comp  f g))
  (-> my-list-functor (fmap g) (fmap f))

  )

