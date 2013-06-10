x(ns edn-play.geometry
  (:require [clojure.math.numeric-tower :as math]))

;; simple representaion of a complex number
;; as a map {:re 0 :im 1}
(defn complex
  "create a complex number in rect form
given x and y"
  [x y]
  {:re x
   :im y})

(defn polar
  "create a complex number in polar form"
  [length angle]
  {:r length
   :a angle})

(defn unit-polar
  "polar form unit from given angle"
  [angle]
  (polar 1 angle))

(def zero (complex 0 0))
(def one (complex 1 0))
(def i (complex 0 1))

(def pi Math/PI)
(def tau (* 2 pi))
(def e Math/E)


(defn len-square
  "return length squared of complex z in rect from"
  [z]
  (let [x (:re z)
        y (:im z)
        r (+ (* x x) (* y y))]
    r))

(defn length [z]
  (math/sqrt (len-square z)))

(defn arg [z]
  (let [x (:re z)
        y (:im z)]
    (Math/atan2 y x)))

(defn plus [z w]
  {:re (+ (:re z) (:re w))
   :im (+ (:im z) (:im w))})

(defn times
  "scalar multiplication"
  [r z]
  {:re (* r (:re z))
   :im (* r (:im z))})

(defn conj-rect
  "conjugate of rect from"
  [z]
  (complex (:re z) (* -1 (:im z))))

(defn conj-polar
  "conjugate of polar form"
  [z]
  (polar (:r z) (* -1 (:a z))))

(defn mult-rect
  "multiply two complex numbers in rectangular form"
  [z1 z2]
  {:re (- (* (:re z1) (:re z2)) (* (:im z1) (:im z2)))
   :im (+ (* (:re z1) (:im z2)) (* (:im z1) (:re z2)))})

(defn mult-polar
  "multipliaction of complex numbers
in polar form"
  [& zs]
  {:r (reduce * (map :r zs))
   :a (mod tau (reduce + (map :a zs)))})

(defn fromPolar
  "given length and angle, return complex in rect form"
  [r theta]
  {:re (* r (Math/cos theta))
   :im (* r (Math/sin theta))})

(defn polar->rect
  "convert polar form to rect"
  [polar-form]
  (fromPolar (:r polar-form) (:a polar-form)))

(defn rect->polar
  "convert rect to polar form"
  [z]
  {:r (length z)
   :a (arg z)})

(def degrees
  "return degrees of rect from, rounded"
  (comp math/round #(Math/toDegrees %) :a rect->polar))

(defn degrees-polar [z]
  ((comp math/round #(Math/toDegrees %) :a) z))

(def radii
  "return length of rect form, rounded"
  (fn [r]
    (-> r
        rect->polar
        :r
        (* 100)
        math/round
        (/ 100.0))))

(defn unit
  "given angle, return unit complex in rect form"
  [angle]
  (fromPolar 1 angle))

(defn inverse [z]
  (fromPolar (/ 1 (length z)) (* -1 (arg z))))

(defn inverse-rect
  "given z in rect form return 1/z
without converting to polar form"
  [z]
  (let [r (len-square z)]
    {:re (/ (:re z) r)
     :im (* -1 (/ (:im z) r))}))

(defn div-rect
  "given z1 z2 in rect form
return z1/z2 without converting to polar form"
  [z1 z2]
  (let [r (len-square z2)]
    {:re (/ (+ (* (:re z1) (:re z2)) (* (:im z1) (:im z2))) r)
     :im (/ (- (* (:im z1) (:re z2)) (* (:re z1) (:im z2))) r)}))

(defn div-rect2
  "divison as multiplication of inverse
for complex numbers in rect-form"
  [z1 z2]
  (let [z2i (inverse-rect z2)]
    (mult-rect z1 z2i)))

(defn nth-roots
  "return nth roots of unity in polar form"
  [n]
  (map (comp unit-polar (partial * tau (/ 1 n))) (range 1 (+ 1 n))))


(comment
  ;; generate some data
  ;; n = 4
  (def s1 (nth-roots 4))
  (apply mult-polar s1)
  ;;=> {:r 1, :a 6.283185307179586}
  (def s2 (map polar-rect s1))
  (map degrees-polar (nth-roots 2))
  )
