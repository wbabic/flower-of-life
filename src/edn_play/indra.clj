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

(def pi Math/PI)
(def tau (* 2 pi))
(def e Math/E)

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
  (comp (t b) (s a)))

(defn affine-inverse [a b]
  "z -> (1/a)z - b
the inverse function of
z -> az + b"
  (comp (s (inverse a)) (t (minus b))))

;; TODO - make an affine record type
;; allowing for composition of elements

(comment
  (def a (make-polar 1 0))
  (def a1 (s a)) ;; identity
  (def a2 (s i)) ;; mult by i is rotation by pi/4

  (a1 one) ;;=> #edn_play.complex.ComplexPolar{:r 1, :a 0.0}
  (a2 one) ;;=> #edn_play.complex.ComplexRect{:x 0, :y 1}
  (a2 (a2 one)) ;;=> #edn_play.complex.ComplexRect{:x -1, :y 0}
  (a2 (a2 (a2 one))) ;;=> #edn_play.complex.ComplexRect{:x 0, :y -1}
  (a2 (a2 (a2 (a2 one)))) ;;=> #edn_play.complex.ComplexRect{:x 1, :y 0}

  (def z2 (make-rect 1 1))
  (map (juxt length angle) (take 5 (iterate a2 z2)))
  (map str (take 5 (iterate a2 z2)))

  (def z3 (make-rect 0.75 0.25))
  (take 5 (iterate a2 z3))

  (def z4 (make-rect 1 2))
  (take 5 (iterate a2 z4))
  )

(comment
  ;; add by 1 + 2i
  (def t1 (t (make-rect 1 2)))
  (take 5 (iterate t1 one))
  (take 5 (iterate t1 i))
  (take 5 (iterate t1 (make-polar 1 (/ tau 4))))
  )

(comment
  ;; now combine rotation and translation
  ;; into one operation
  ;; a = i
  ;; b = 1 + 2i

  (def zero (make-rect 0 0))
  ((t (make-rect 1 2)) zero)
  (str ((t (make-polar 1 (/ tau 4))) zero))
  ;;=> "0.0 + i1.0"
  ((s i) zero)
  ;;=> #edn_play.complex.ComplexRect{:x 0, :y 0}

  (def a3 (affine i (make-rect 1 2)))
  (a3 zero)
  ;;=> #edn_play.complex.ComplexRect{:x 1, :y 2}

  ;; fixed point -1/2 + 3/2 i
  (a3 (make-rect (- (/ 2)) (/ 3 2)))
  ;;=> #edn_play.complex.ComplexRect{:x -1/2, :y 3/2}

  ;; -i -> 1 -> 2 + 2i
  (a3 (make-rect 0 (- 1)))
  ;;=> #edn_play.complex.ComplexRect{:x 2, :y 2}
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

