(ns delaunay.utils.circle
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.core.vector :refer [vec2]]
            [delaunay.utils.matrix33 :refer [matrix33]]))

(defn determinant
  [m]
  #+clj (.determinant m)
  #+cljs (.call delaunay.utils.matrix33.Matrix33.prototype.thi$ng$geom$core$PDeterminant$determinant$arity$1 m))

(defn mag-squared
  [p]
  #+clj (.mag-squared p)
  #+cljs (g/mag-squared p))

(defn center-and-radius
  "Calculate center and radius of the circle passing through p1, p2, and p3."
  [p1 p2 p3]
  (let [a (determinant (matrix33 (.-x p1) (.-y p1) 1
                                  (.-x p2) (.-y p2) 1
                                  (.-x p3) (.-y p3) 1))
        d (- (determinant (matrix33 (mag-squared p1) (.-y p1) 1
                                     (mag-squared p2) (.-y p2) 1
                                     (mag-squared p3) (.-y p3) 1)))
        e (determinant (matrix33 (mag-squared p1) (.-x p1) 1
                                  (mag-squared p2) (.-x p2) 1
                                  (mag-squared p3) (.-x p3) 1))
        f (- (determinant (matrix33 (mag-squared p1) (.-x p1) (.-y p1)
                                     (mag-squared p2) (.-x p2) (.-y p2)
                                     (mag-squared p3) (.-x p3) (.-y p3))))
        center (vec2 (- (/ d (* 2 a))) (- (/ e (* 2 a))))
        radius (Math/sqrt (- (/ (+ (* d d) (* e e)) (* 4 (* a a))) (/ f a)))]
    [center radius]))
