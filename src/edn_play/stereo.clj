(ns edn-play.stereo
  (:require [edn-play.geometry :as geom]))

;; stereogrphic projection  mapping
;; from complex plane to init sphere

;; vector operators
(defn vec-add [& args] (vec (apply map + args)))
(defn vec-sub [& args] (vec (apply map - args)))
(defn vec-dot [u v] (reduce + (map * u v)))
(defn vec-length [v] (Math/sqrt (vec-dot v v)))
(defn vec-scale [r v] (vec (map #(* r %) v)))
(defn vec-normalize [v] (vec-scale (/ 1 (vec-length v)) v))

;; quaterninons
(defn det-2x2 [a b c d] (- (* a d) (* b c)))
(defn vec-cross-3d [u v]
  (vector (det-2x2 (u 1) (u 2) (v 1) (v 2))
          (det-2x2 (u 2) (u 0) (v 2) (v 0))
          (det-2x2 (u 0) (u 1) (v 0) (v 1))))
(defn quat-mul [q1 q2]
  (let [r1 (first q1) v1 (vec (rest q1))
        r2 (first q2) v2 (vec (rest q2))]
    (cons (- (* r1 r2) (vec-dot v1 v2))
          (vec-add (vec-scale r1 v2)
                   (vec-scale r2 v1)
                   (vec-cross-3d v1 v2)))))

(defn to-plane
  "project from sphere to plane"
  [[x y z]]
  (let [p (- 1 z)]
    [(/ x p) (/ y p)]))

(defn to-sphere
  "project from plane to sphere"
  [[u v :as r]]
  (let [p (vec-dot r r)
        q (+ 1 p)]
    [(/ (* 2 u) q)
     (/ (* 2 v) q)
     (/ (- 1 p) q)]))

(comment
  ;; south pole -> origin
  (to-plane [0 0 -1])
  ;;=> [0 0]

  ;; equator to unit circle
  (to-plane [1 0 0])
  ;;=> [1 0]
  (to-plane [0 1 0])
  ;;=> [0 1]
  (def p1 (vec-normalize [1 1 0]))
  (vec-length p1)
  ;;=> 0.9999999999999999
  (def q1 (to-plane p1))
  (vec-length q1)
  ;;=> 0.9999999999999999
  
  (to-sphere [1 0])
  ;;=> [1 0 0]

  (to-sphere [100 0])
  ;;=> [200/10001 0 -9999/10001]

  (to-sphere [(float (/ 1 100)) 0])
  ;;=> [0.019997999753079258 0.0 0.9998000200069391]


  (to-plane (to-sphere [100 0]))
  [1/100 0N]

  (to-sphere (to-plane (vec-normalize [1 1 1])))
  )
