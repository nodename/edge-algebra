(ns edge-algebra.delaunay
  (:require [adge-algebra.core :refer [new-edge! splice!]]))


;; For the Delaunay application, the data field of an Edge will contain the coordinates of its origin:
(defn org
  [edge]
  (.getData edge))

(defn set-org!
  [edge coords]
  (.setData edge coords))

;; and similarly:
(defn dest
  [edge]
  (org (sym edge)))

(defn set-dest!
  [edge coords]
  (.setData (sym edge) coords))


(defn connect!
  [a b side] ;; side?
  (let [e (new-edge!)]
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
