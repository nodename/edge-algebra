(ns edge-algebra.core
  (:require [edge-algebra.edge-record :refer [make-edge-record]]
            [edge-algebra.edge :refer [rot flip origin-vertex dest-vertex left-face right-face get-edge]]))

(defn new-edge
  ;; TODO make a subdivision
  []
  (let [er (make-edge-record)]
    (get-edge er 0 0)))


;; the fully qualified types:
(def Edge edge_algebra.edge.Edge)
(def Node edge_algebra.node.Node)


;; node roles with respect to an edge:
(def RIGHT-FACE 0)
(def DEST-VERTEX 1)
(def LEFT-FACE 2)
(def ORIGIN-VERTEX 3)


(defmulti dual
  "The dual of an Edge or a Node is its counterpart in the dual subdivision."
  class)


(defmethod dual Edge
  [edge]
  (rot (flip edge)))

(defmethod dual Node
  [node]
  (let [an-edge (get-edge (.getEdgeRecord node) 0 0)
        node-relationship-to-edge (mod (- (.-r node) (.-r an-edge)) 4)
        dual-edge (dual an-edge)
        dual-node-relationship-to-dual-edge (condp = node-relationship-to-edge
                                              RIGHT-FACE (if (zero? (.-f node))
                                                            dest-vertex
                                                            origin-vertex)

                                              DEST-VERTEX right-face

                                              LEFT-FACE (if (zero? (.-f node))
                                                          origin-vertex
                                                          dest-vertex)

                                              ORIGIN-VERTEX left-face)]

    (dual-node-relationship-to-dual-edge dual-edge)))
