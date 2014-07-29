(ns edge-algebra.core
  (:require [edge-algebra.edge-record :refer [new-edge-record!]]
            [edge-algebra.record :refer [get-e0]]
            [edge-algebra.edge :refer [o-next rot]]))


(defn make-edge!
  ;; TODO make a subdivision
  []
  (get-e0 (new-edge-record!)))


(defn splice!
  [edge0 edge1]
  (let [edge0-next (o-next edge0)
        edge1-next (o-next edge1)
        alpha (rot edge0-next)
        beta (rot edge1-next)
        alpha-next (o-next alpha)
        beta-next (o-next beta)]
    (.setNext edge0 edge1-next)
    (.setNext edge1 edge0-next)
    (.setNext alpha beta-next)
    (.setNext beta alpha-next)))





