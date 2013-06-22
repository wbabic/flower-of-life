(ns edn-play.indra
  (:use edn-play.complex)
  (:require [clojure.math.numeric-tower :as math])
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
  "z -> 1/a(z - b)
the inverse function of
z -> az + b"
  (comp (s (inverse a)) (t (minus b))))

(defn invert-by
  "return function that inverts in circle
with given radius and center
radius a real number and circle a complex number"
  [radius center]
  (let [a (make-rect radius 0)
        b center]
    (comp
     (affine a b)
     (inversion-by)
     (affine-inverse a b))))

(comment
  ;; test invert-by radius = 2, caenter = (1,1)
  (def r 2)
  (def b (make-rect 1 1))
  ((affine (make-rect r 0) b) (make-rect 0 0))
  ;; (0,0) -> (1,1)
  ((affine (make-rect r 0) b) (make-rect 1 0))
  ;; (1,0) -> (3,1)
  ((affine-inverse (make-rect r 0) b) (make-rect 1 1))
  ;; (1, 1) -> (0, 0)
  ((affine-inverse (make-rect r 0) b) (make-rect 3 1))
  ;; (3,1) -> (1, 0)

  ((add-by (minus b)) (make-rect 3 1))
  (polar (make-rect r 0))
  (polar (inverse (make-rect r 0)))
  ;;=> #edn_play.complex.ComplexPolar{:r 1/2, :a 0.0}
  
  (def invert-1 (invert-by r b))
  ((invert-by r b) (make-rect 3 1))
  ;;=> #edn_play.complex.ComplexRect{:x 3N, :y 1N}
  
  (def test-points [[2 1] [3 1] [4 1] [5 1] [1 2] [1 3] [1 4]])
  (def test-data (map #(apply make-rect %) test-points))

  (invert-1 (first test-data))
  #edn_play.complex.ComplexRect{:x 5N, :y 1N}

  (map (comp str invert-1) test-data)
  )
;; TODO - make an affine record type
;; allowing for composition of elements

(comment
  (def b1 (make-polar 1 0))
  (def a1 (s b1)) ;; identity
  (def a2 (s i)) ;; mult by i is rotation by pi/4

  (a1 one) ;;=> #edn_play.complex.ComplexPolar{:r 1, :a 0.0}
  (a2 one) ;;=> #edn_play.complex.ComplexRect{:x 0, :y 1}
  (a2 (a2 one)) ;;=> #edn_play.complex.ComplexRect{:x -1, :y 0}
  (a2 (a2 (a2 one))) ;;=> #edn_play.complex.ComplexRect{:x 0, :y -1}
  (a2 (a2 (a2 (a2 one)))) ;;=> #edn_play.complex.ComplexRect{:x 1, :y 0}

  (def z3 (make-rect 1 1))
  (map (juxt length angle) (take 5 (iterate a2 z3)))
  (map str (take 5 (iterate a2 z3)))

  (def z4 (make-rect 0.75 0.25))
  (take 5 (iterate a2 z4))

  (def z5 (make-rect 1 2))
  (take 5 (iterate a2 z5))
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
  ;;=> #edn_play.complex.ComplexRect{:x 1, :y 2}
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

;; the geometry og complex maps
;; z -> z^2
;; z -> z^(1/2)
;; z -> 1/z
;; images of grid lines

;; prepare the data to be mapped
;; then map into canvas elements
;; start with a patch
;; from (-1,-1) to (1,1)
;; with xres vertical lines
;; and yres horizontal lines

(def xrange {:start -1 :end 1 :inc 0.5})

(def yrange {:start -1 :end 1 :inc 0.5})

(defn horizontal-lines [xrange yrange]
  (vec
   (for [x (range (:start xrange) (+ (:inc xrange) (:end xrange)) (:inc xrange)) ]
     (vec
      (for [y (range (:start yrange) (+ (:inc yrange) (:end yrange)) (:inc yrange))]
        [x y])))))

(defn vertical-lines [xrange yrange]
  (vec
   (for [y (range (:start yrange) (+ (:inc yrange) (:end yrange)) (:inc yrange)) ]
     (vec
      (for [x (range (:start xrange) (+ (:inc xrange) (:end xrange)) (:inc xrange))]
        [x y])))))

(def h-lines (horizontal-lines xrange yrange))

(def v-lines (vertical-lines xrange yrange))

(defn pre-image [lines]
  (vec
   (for [line lines]
     (mapv #(apply make-rect %) line))))

(defn image [pre-image f]
  (vec
   (for [line pre-image]
     (mapv f line))))

(def f1 (fn [z] (mult z z)))

(def f2 (fn [z]
          (let [r (length z)
                a (angle z)]
            (make-polar (math/sqrt r) (/ a 2)))))

(def f3 (fn [z] (inverse z)))

(defn max-grid [grid]
  (apply max (map length (flatten grid))))

(comment
  (def g1 (pre-image h-lines))
  (def fg1 (image g1 f1))
  (def fg2 (image g1 f2))
  (def fg3 (image g1 f3))
  (max-grid g1)
  (max-grid fg1)
  (max-grid fg2)
  (max-grid fg3)
  
  (map (comp str polar) (flatten (image g1 f1)))
  
  )

;; 2.6
;; a tiling associated to inversion
(defn make-element [name value-fn]
  {:name name
   :fn (fn [z] (value-fn z))})

(def g [(make-element :t1 identity)
        (make-element :t2 conjugate)
        (make-element :t3 minus)
        (make-element :t4 (comp minus conjugate))
        (make-element :t5 inverse)
        (make-element :t6 (comp inverse conjugate))
        (make-element :t7 (comp inverse minus))
        (make-element :t8 (comp inverse minus conjugate))])

(comment
  (for [f (map :fn g)] (str (f (make-rect 1 1))))
  ;;=> ("1.0 + i1.0" "1.0 + i-1.0" "-1.0 + i-1.0" "-1.0 + i1.0" "0.5 +
  ;;i-0.5" "0.5 + i0.5" "-0.5 + i0.5" "-0.5 + i-0.5")

  (for [f (map :fn g)] (str (f i)))
  ;;=> ("0.0 + i1.0" "0.0 + i-1.0" "0.0 + i-1.0" "0.0 + i1.0" 
  ;;=> "0.0 + i-1.0" "0.0 + i1.0" "0.0 + i1.0" "0.0 + i-1.0")

  (= i (make-rect 0 1))
  ;;=> true
  (= i (make-polar 1 (/ pi 4)))
  ;;=> false

  (defn equal [z w]
    (and (= (length z) (length w))
         (= (angle z) (angle w))))
  (equal i (make-polar 1 (/ pi 2)))
  ;;=> true

  ;; TODO deal with infinity
  (inversion zero) ;;=> infinity
  ;; infinty implementing complex protocol
  ;; map infinity to north pole

  ;; TODO form the compostion of two maps
  ;; returning another map
  ;; (comp (:fn :t2) (:fn :t3)) -> (:fn :t4)
  ;; problen: how to detrmine if two maps are equal?
  )
