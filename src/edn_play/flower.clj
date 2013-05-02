(ns edn-play.flower
  (:require [edn-play.geometry :as geom]))

(defn circle-set [name circles]
  {:name name
   :circles (vec circles)})

;; circle sets
;; the zeroth circle and set
(def c0 geom/zero)
(def s0 (circle-set :c0 c0))

;; the first circle set
(def base (map (comp geom/unit #(* (/ geom/tau 6) %)) ( range 6)))

(def c1 base)
(def s1 (circle-set :c1 c1))

;; the second set
(defn shift
  "shift base one position"
  [n base]
  (take 6 (drop n (cycle base))))

(def b+1 (shift 1 base))
(def c2 (map geom/plus c1 b+1))
(def s2 (circle-set :c2 c2))

;; the third set
(def b+2 (shift 2 base))
(def c3 (map #(geom/times 2 %) c1))
(def s3 (circle-set :c3 c3))

;; the fourth set has 12
;; two for each in third
(def b+5 (shift 5 base)) ;; same as b-1
(def c4 (interleave (map geom/plus c3 b+5)
                    (map geom/plus c3 b+1)))

(def s4 (circle-set :c4 c4))

;; the fifth set
(def c51 (map #(geom/times 3 %) c1))
(def s51 (circle-set :c51 c51))

(def even-c4 (take-nth 2 c4))
(def odd-c4 (take 6 (take-nth 2 (drop 1 (cycle c4)))))
(def c52 (map geom/plus odd-c4 b+1))
(def s52 (circle-set :c52 c52))

(def c6 (map geom/plus c4 (interleave base base)))
(def s6 (circle-set :c6 c6))

(def c7 (map #(geom/times 4 %) c1))
(def s7 (circle-set :c7 c7))

(def circle-set [s0 s1 s2 s3 s4 s51 s52 s6 s7])
