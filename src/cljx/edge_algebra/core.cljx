;; ## A data structure for generalized diagrams
;;
;; "Generalized diagrams -- that is, embeddings of graphs in two-dimensional
;; manifolds. This structure represents simultaneously an embedding, its dual,
;; and its mirror image. Furthermore, just two operators are sufficient
;; for building and modifying arbitrary diagrams."
;;
;; ## Some Topology
;;
;; "In mathematics, the closure of a subset S in a topological space
;; consists of all points in S plus the limit points of S. The closure of S
;; is also defined as the union of S and its boundary." -- Wikipedia
;;
;; Homeomorphisms are the isomorphisms in the category of topological spaces.
;;
(ns edge-algebra.core
  (:require [edge-algebra.edge-record :refer [new-edge-record!]]
            [edge-algebra.record :refer [get-e0]]
            [edge-algebra.app-state :refer [set-next!]]
            [edge-algebra.edge :refer [o-next rot]]))

;; ## The Two Operators Exported by the Library
;; <img src="make-edge.jpg" />

(defn make-edge!
  []
  (get-e0 (new-edge-record!)))

;; <img src="splice-1.jpg" />
;; <img src="splice-2.jpg" />

(defn splice!
  "splice! is its own inverse!"
  [edge0 edge1]
  (let [edge0-next (o-next edge0)
        edge1-next (o-next edge1)
        alpha (rot edge0-next)
        beta (rot edge1-next)
        alpha-next (o-next alpha)
        beta-next (o-next beta)]
    (set-next! edge0 edge1-next)
    (set-next! edge1 edge0-next)
    (set-next! alpha beta-next)
    (set-next! beta alpha-next)))





