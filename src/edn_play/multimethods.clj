(ns edn-play.multimethods)

(comment
  (doc isa?)
  ::rect

  (derive ::rect ::shape)
  (derive ::square ::rect)
  (doc parents)
  (doc derive)

  (parents ::rect)
  (descendants ::shape)
  (ancestors ::square)

  (isa? 42 42)
  (isa? ::square ::shape)

  (derive java.util.Map ::collection)
  (derive java.util.Collection ::collection)
 
  (isa? java.util.HashMap ::collection)
  ;;=> true

  (isa? String Object)
  (isa? [::square ::rect] [::shape ::shape])

  (ancestors java.util.ArrayList)

  (defmulti foo class)
  (defmethod foo ::collection [c] :a-collection)
  (defmethod foo String [s] :a-string)
 
  (foo [])
  :a-collection
 
  (foo (java.util.HashMap.))
  :a-collection
 
  (foo "bar")
  :a-string


  (defmulti bar (fn [x y] [x y]))
  (defmethod bar [::rect ::shape] [x y] :rect-shape)
  (defmethod bar [::shape ::rect] [x y] :shape-rect)

  (bar ::rect ::rect)

  (prefer-method bar [::rect ::shape] [::shape ::rect])
  (bar ::rect ::rect)

  (doc make-hierarchy)


  (defmulti area :Shape)
  (defn rect [wd ht] {:Shape :Rect :wd wd :ht ht})
  (defn circle [radius] {:Shape :Circle :radius radius})
  (defmethod area :Rect [r]
    (* (:wd r) (:ht r)))
  (defmethod area :Circle [c]
    (* (. Math PI) (* (:radius c) (:radius c))))
  (defmethod area :default [x] :oops)
  (def r (rect 4 13))
  (def c (circle 12))
  (area r)

  (area c)

  (area {})

  )

(comment
  ;; protocols
  (defprotocol P
    (foo [x])
    (bar-me [x] [x y]))

  (deftype Foo [a b c]
    P
    (foo [x] a)
    (bar-me [x] b)
    (bar-me [x y] (+ c y)))
 
  (bar-me (Foo. 1 2 3) 42)

  (foo
   (let [x 42]
     (reify P
       (foo [this] 17)
       (bar-me [this] x)
       (bar-me [this y] x))))
  
  )
