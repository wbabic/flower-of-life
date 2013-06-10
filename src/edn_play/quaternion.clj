(ns edn-play.quaternion
  (:require [edn-play.geometry :as geom]))

;; from visualizing quaternions

;; quaternion
;; multiplication, cross product, inner product, conjugate

;; representation

;; as a four vector
;; with real part and a 3d imaginary part

(defn quaternion [p0 p1 p2 p3]
  [p0 p1 p2 p3])

(def one
  "unit quaternion"
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
  [q]
  (let [l (length q)]
    (apply quaternion (scale (/ 1 l) q))))
