(ns edge-algebra.core
  (:require [edge-algebra.edge-record :refer [make-edge-record!]]
            [edge-algebra.edge :refer [o-next rot get-edge]]))


(defn new-edge!
  ;; TODO make a subdivision
  []
  (let [er (make-edge-record!)]
    (get-edge er 0 0)))

;; This is where the magic happens:

(defn splice!
  [edge0 edge1]
  (let [edge0-next (o-next edge0)
        edge1-next (o-next edge1)
        alpha (rot edge0-next)
        beta (rot edge1-next)
        alpha-next (o-next alpha)
        beta-next (o-next beta)]
    (.setNextRef edge0 edge1-next)
    (.setNextRef edge1 edge0-next)
    (.setNextRef alpha beta-next)
    (.setNextRef beta alpha-next)))





