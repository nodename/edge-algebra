(ns edge-algebra.delaunay
  (:require
   ;; the two topological operations exported by the edge-algebra library:
   [edge-algebra.core :refer [make-edge! splice!]]
   ;; some neighboring-edge properties:
   [edge-algebra.edge :refer [sym l-next o-prev]]

   ;; geometric support from toxi's geom library:
   [thi.ng.geom.core.matrix :refer [matrix44]]
   [thi.ng.geom.core.utils :refer [clockwise2?]])

  (:refer-clojure :exclude [swap!]))


;; For the Delaunay application, the data field of an Edge will contain the coordinates of its origin.
(defn org
  [edge]
  (.getData edge))

(defn set-org!
  [edge coords]
  (.setData edge coords))

;; and similarly, the data field of its symmetric Edge will contain the coordinates of its destination.
(defn dest
  [edge]
  (org (sym edge)))

(defn set-dest!
  [edge coords]
  (.setData (sym edge) coords))




(defn connect!
  [a b side] ;; side?;
  (let [e (make-edge!)]
    (set-org! e (dest a))
    (set-dest! e (org b))
    (splice! e (l-next a))
    (splice! (sym e) b)
    e))

(defn delete-edge!
  [e]
  (splice! e (o-prev e))
  (splice! (sym e) (o-prev (sym e))))


;; for the incremental algorithm:
(defn swap!
  [e]
  (let [a (o-prev e)
        b (o-prev (sym e))]
    (splice! e a)
    (splice! (sym e) b)
    (splice! e (l-next a))
    (splice! (sym e) (l-next b))
    (set-org! e (dest a))
    (set-dest! e (dest b))))




(defn in-circle
  "The predicate in-circle(a, b, c, d) is defined to be true if and only if
  point d is interior to the region of the plane that is bounded by the
  oriented circle abc and lies to the left of it."
  [a b c d]
  (let [m (matrix44 (.-x a) (.-y a) (.mag-squared a) 1
                    (.-x b) (.-y b) (.mag-squared b) 1
                    (.-x c) (.-y c) (.mag-squared c) 1
                    (.-x d) (.-y d) (.mag-squared d) 1)]

    (> (.determinant m) 0)))


(defn ccw
  "The predicate ccw is true if the points a, b, and c form a counterclockwise-oriented triangle."
  [a b c]
  (not (clockwise2? a b c)))

(defn right-of
  [point edge]
  (ccw point (dest edge) (org edge)))

(defn left-of
  [point edge]
  (ccw point (org edge) (dest edge)))

(defn delaunay
  [sites]
  (condp = (count sites)
    1 nil
    2 (let [a (new-edge!)]
        (set-org! a (sites 0))
        (set-dest! a (sites 1))
        [a (.sym a)])
    3 (let [[a b c] (sort sites)])
    ))


