(ns edn-play.flower
  (:require [edn-play.geometry :as geom]
            [clojure.math.combinatorics :as combo]))

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
(def tree-set [s0 s3 s7])


;; metatron's cube
;; inner inner-to-outer outer
;; {:name :lines [[p1 p2] [p3 p4] ...]}

(defn line-set [name lines]
  {:name name
   :lines lines})

(def inner (line-set :inner (vec (map vec (combo/combinations c3 2)))))

(def outer (line-set :outer (vec (map vec (combo/combinations c7 2)))))

;; outer-to-inner
(def o-to-i (line-set :outerToInner (reduce into [] (for [po c7] (for [pi c3] [po pi])))))

(def metatron [inner outer o-to-i])

;; 4 tetraaheda; inner outer up down
;; tiu tid tou tod
(comment
  "sample data set"
  {:name :tiu
   :faces [{:name :tiu0
            :points [i2 i4 i6]
            :color (0,0,0,0)}]})

(defn up-faces [d]
  [[c0 (nth d 0) (nth d 2)]
   [c0 (nth d 2) (nth d 4)]
   [c0 (nth d 4) (nth d 0)]
   [(nth d 0) (nth d 2) (nth d 4)]])

(defn down-faces [d]
  [[c0 (nth d 1) (nth d 3)]
   [c0 (nth d 3) (nth d 5)]
   [c0 (nth d 5) (nth d 1)]
   [(nth d 1) (nth d 3) (nth d 5)]])


(def color-set1 [:r :g :b :w])
(def color-set2 [:m :c :y :w])


(defn make-tetrahedron-faces [namek data color]
  (let [i (count data)
        base-name (name namek)]
    (map
     (fn [j d c]
       {:name (keyword (str base-name j))
        :points d
        :color c})
     (range i) data color)))

(defn make-tetrahedron [name data color]
  {:name name
   :faces (vec (make-tetrahedron-faces name data color))})

(def tiu (make-tetrahedron :tiu (up-faces c3) color-set1))
(def tid (make-tetrahedron :tid (down-faces c3) color-set2))
(def tou (make-tetrahedron :tou (up-faces c7) color-set1))
(def tod (make-tetrahedron :tod (down-faces c7) color-set2))

(def tetrahedra [tiu tid tou tod])
