(ns edn-play.indra
  (:use edn-play.complex)
  (:import  (edn_play.complex.ComplexRect)
            (edn_play.complex.ComplexPolar)))

;; indra's pearls
;; the vision of felix klein

(def ds1 "0 to 2+2i integer coordinate grid"
  (for [j (range 3) k (range 3)]
    (edn_play.complex.ComplexRect. j k)))

(def ids1 "multiply ds1 by 1 + 1i "
  (let [k (edn_play.complex.ComplexRect. 1 1)
        a (mult-by k)]
    (map a ds1)))

;; complex transform
;; Sa: z -> az
;; Tb: z -> z + b
;; (conjugate Sa Tb) : z -> z-b -> a(z-b) -> (a(z-b) + b
;; affine: z -> az + b
;; a != 0, affine group of maps

(defn s [a]
  (mult-by a))

(defn t [b]
  (add-by b))

(defn affine [a b]
  "z -> az + b"
  (comp (s b) (t a)))

(defn affine-inverse [a b]
  "z -> (1/a)z - b
the inverse function of
z -> az + b"
  (comp (s (inverse a)) (t (minus b))))

;; TODO - make an affine record type
;; allowing for composition of elements
(comment
  (def one (make-rect 1 0))
  (def b (make-rect 1 2))
  (def a (polar 1 (/ Math/PI 4)))
  (def a1 (affine a b))
  (def c (a1 (rect 1 0)))
  ((s a) one)
  ;;=> #edn_play.complex.ComplexRect{:x 1, :y 2}
  (def z (make-rect 1 1))

  (str (a1 one))
  ;;=> "2.83e^i0.79"
  (def a2 (affine-inverse a b))
  ((comp a2 a1) z)
  )
;; inversion in a circle
;; z -> 1 / (z bar)
;; inversion in unit circle I
;; (comp inverse conjugate)
;; inversion in circle with radius R, center a
;; conjuagte with T: z -> Rz + a
;; (conjuate I T): (comp T I (inverse T))

;; ch 2 projects

;; cxsqrt
;; complex square roots
;; using only real square roots of real positive numbers
;; no atan2


;; the geometry og complex maps
;; z -> z^2
;; z -> z^(1/2)
;; z -> 1/z
;; images of grid lines

(defn cxsqrt [z])


;; 2.4
;; affine maps

;; 2.5
;; discrete and continuous spirals

;; 2.6
;; a tiling associated to inversion

