(ns edn-play.geometry
  (:require [clojure.math.numeric-tower :as math]))

;; simple representaion of a complex number
;; as a map {:re 0 :im 1}

(def zero {:re 0 :im 0})
(def one {:re 1 :im 0})
(def i {:re 0 :im 1})

(defn length [z]
  (let [x (:re z)
        y (:im z)]
    (math/sqrt (+ (* x x) (* y y)))))

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

(defn fromPolar [r theta]
  {:re (* r (Math/cos theta))
   :im (* r (Math/sin theta))})

(defn toPolar [z]
  {:r (length z)
   :a (arg z)})

(def degrees
  (comp math/round #(Math/toDegrees %) :a toPolar))

(def radii
  (fn [r]
    (-> r
        toPolar
        :r
        (* 100)
        math/round
        (/ 100.0))))

(defn unit [angle]
  (fromPolar 1 angle))

(def tau (* 2 Math/PI))

(defn inverse [z]
  (fromPolar (/ 1 (length z)) (* -1 (arg z))))

