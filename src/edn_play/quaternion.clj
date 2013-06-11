(ns edn-play.quaternion
  (:require [edn-play.geometry :as geom]))

;; from visualizing quaternions

;; quaternion
;; multiplication, cross product, inner product, conjugate

;; representation

;; as a four vector
;; with real part and a 3d imaginary part

(defn quaternion
  [p0 p1 p2 p3]
  [p0 p1 p2 p3])

(def one
  "identity quaternion"
  (quaternion 1 0 0 0))

(defn re [q]
  (first q))

(defn im [q]
  (vec (rest q)))

(defn scale [r v]
  (map #(* r %) v))

(defn add [& vs]
  (apply map + vs))

(defn dot [p q] (reduce + (map * p q)))

(defn conjugate [q]
   (apply quaternion (re q) (map #(* -1 %) (im q))))

(defn det-2x2 [a b c d] (- (* a d) (* b c)))

(defn cross-3d [u v]
  (vector (det-2x2 (u 1) (u 2) (v 1) (v 2))
          (det-2x2 (u 2) (u 0) (v 2) (v 0))
          (det-2x2 (u 0) (u 1) (v 0) (v 1))))

(defn mult [p q]
  (let [p0 (re p) p3 (im p)
        q0 (re q) q3 (im q)]
    (apply quaternion
           (- (* p0 q0) (dot p3 q3))
           (add (scale p0 q3)
                (scale q0 p3)
                 (cross-3d p3 q3)))))

(defn length-squared
  "length squared of a quaternion"
  [q]
  (dot q q))

(defn length
  "length of a quaternion"
  [q]
  (Math/sqrt (length-squared q)))

(defn normalize
  "normalized given non zero quaternion"
  ([q]
     (let [l (length q)]
       (apply quaternion (scale (/ 1 l) q))))
  ([x y z]
     (let [v (vector x y z)
           l (length v)]
       (vec (scale (/ 1 l) v)))))

(defn unit
  "represent a rotaion by angle about normal"
  ([angle n1 n2 n3]
     (unit angle (vector n1 n2 n3)))
  ([angle normal]
     (let [theta  (/ angle 2)
           c (Math/cos theta)
           s (Math/sin theta)]
       (apply quaternion c (map #(* s %) normal)))))

(comment
  (length (unit geom/pi (normalize 1 1 1)))
  ;;=> 1.0
  (mult (normalize p) (normalize (conjugate p)))
  ;;=> [0.9999999999999998 0.0 0.0 0.0]
  (length (mult (normalize p) (normalize (conjugate p))))
  ;;=> 0.9999999999999998
  (length (normalize p))
  ;;=> 0.9999999999999999
  )

(defn rotation
  "3d rotation matrix about normal by angle
defined by unit quaternion"
  [[a b c d]]
  (let [a2 (* a a) b2 (* b b) c2 (* c c) d2 (* d d)
        ab (* a b) ac (* a c) ad (* a d)
        bc (* b c) bd (* b d)])
  [[(+ a2 b2 (* -1 c2) (* -1 d2)) (* 2 (- bc ad)) (* 2 (+ bd ac))]
   [(* 2 (+ bd ad)) (+ a2 c2 (* -1 b2) (* -1 d2)) (* 2 (- cd ab))]
   [(* 2 (- bd ac)) (* 2 (+ cd ab)) (+ a2 d2 (* -1 b2) (* -1 d2))]])

;; given a rotation matrix
;; find normal and angle
;; explore composition  of two rotations
;; apply rotation matrix to sets of vectors
;; 
