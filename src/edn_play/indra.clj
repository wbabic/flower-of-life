(ns edn-play.indra
  (:use edn-play.geometry))

;; indra's pearls
;; the vision of felix klein

(def ds1 "0 to 2+2i integer coordinate grid"
  (for [j (range 3) k (range 3)]
    (complex j k)))

(def ids1 "multiply ds1 by 1 + 1i "
  (let [k (complex 1 1)]
    (map (partial mult-rect k) ds1)))

;; complex transform
;; Sa: z -> az
;; Tb: z -> z + b
;; (conjugate Sa Tb) : z -> z-b -> a(z-b) -> (a(z-b) + b
;; affine: z -> az + b
;; a != 0, affine group of maps

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

