(ns edn-play.complex
  (:require [clojure.math.numeric-tower :as math]))

;; the complex interface

(defprotocol complex
  (re [complex])
  (im [complex])
  (length [complex])
  (angle [complex])
  (conjugate [complex])
  (polar [complex])
  (rect [complex])
  (inverse [complex])
  (mult [z1 z2])
  (mult-by [z1])
  (plus [z1 z2])
  (minus [z])
  (add-by [z1]))

(defn round [r]
  (-> r (* 100) math/round (/ 100.0)))

(defrecord ComplexRect [x y]
  Object
  (toString [this]
    (str (round (:x this)) " + i" (round (:y this)))))

(defrecord ComplexPolar [r a]
  Object
  (toString [this]
    (str (round (:r this)) "e^i" (round (:a this)))))

(extend-type ComplexRect
  complex
  (re [z] (:x z))
  (im [z] (:y z))
  (conjugate [z] (ComplexRect. (:x z) (- (:y z))))
  (length [z]
    (let [x (:x z)
          y (:y z)]
      (math/sqrt (+ (* x x) (* y y)))))
  (angle [z]
    (let [x (:x z)
          y (:y z)]
      (Math/atan2 y x)))
  (rect [z] z)
  (polar [z] (ComplexPolar. (length z) (angle z)))
  (inverse [z]
    (let [r (length z)
          x (/ (:x z) r)
          y (/ (- (:y z) r))]
      (ComplexRect. x y)))
  (mult [z1 z2]
    (let [x1 (:x z1) y1 (:y z1)
          z2 (rect z2)
          x2 (:x z2) y2 (:y z2)
          x3 (- (* x1 x2) (* y1 y2))
          y3 (+ (* x1 y2) (* y1 x2))]
      (ComplexRect. x3 y3)))
  (plus [z1 z2]
    (let [z2 (rect z2)
          x (+ (:x z1) (:x z2))
          y (+ (:y z1) (:y z2))]
      (ComplexRect. x y)))
  (minus [z]
    (let [x (- (:x z))
          y (- (:y z))]
      (ComplexRect. x y)))
  (mult-by [z1]
    (fn [z2] (mult z1 z2)))
  (add-by [z]
    (fn [w] (plus z w))))

(extend-type ComplexPolar
  complex
  (length [z] (:r z))
  (angle [z] (:a z))
  (re [z] (let [r (:r z) a (:a z)]
            (* r (Math/cos a))))
  (im [z] (let [r (:r z) a (:a z)]
            (* r (Math/sin a))))
  (conjugate [z] (ComplexPolar. (:r z) (- (:a z))))
  (polar [z] z)
  (rect [z] (ComplexRect. (re z) (im z)))
  (inverse [z]
    (let [r (/ (:r z))
          a (- (:a z))]
      (ComplexPolar. r a)))
  (mult [z1 z2]
    (let [r1 (:r z1) a1 (:a z1)
          z2 (polar z2)
          r2 (:r z2) a2 (:a z2)]
      (ComplexPolar. r2 a2)))
  (mult-by [z1]
    (fn [z2] (mult z1 z2)))
  (plus [z1 z2]
    (plus (rect z1) z2))
  (minus [z]
    (minus (rect z)))
  (add-by [z]
    (fn [w] (plus z w))))

(comment
  (str (ComplexPolar. 1 (/ Math/PI 2)))
  ;;=> "1.0e^i1.57"
  (str (inverse (ComplexPolar. 1 (/ Math/PI 2))))
  ;;=> "1.0e^i-1.57"
  (str (inverse (ComplexRect. 1 1)))
  (str (polar (map->ComplexRect {:x 1 :y 2})))
  ;;=> "2.24e^i1.11"

  (def z1 (ComplexRect. 1 1))
  (angle z1)
  ;;=> 0.7853981633974483
  (def a (mult-by z1))

  (def z2 (ComplexPolar. 1 (/ (Math/PI) 2)))
  (angle z2)
  ;;=> 1.5707963267948966
  (angle (a z2))
  ;;=> 2.356194490192345
  ;; multiplication adds angles

  (str (a (ComplexRect. 1 1)))
  ;;=> "1.41e^i0.79"
  
  )

(defn make-rect [x y]
  (ComplexRect. x y))

(defn make-polar [r a]
  (ComplexPolar. r a))

(def one (make-rect 1 0))
(def i (make-rect 0 1))

(comment
  ((mult-by i) one)
  ((add-by one) one)
  )
