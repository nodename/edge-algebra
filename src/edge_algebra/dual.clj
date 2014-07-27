(ns edge-algebra.dual
  (:require [edge-algebra.edge :refer [rot flip
                                       origin-vertex dest-vertex
                                       left-face right-face
                                       get-edge]]))

;; the fully qualified types:
(def Edge edge_algebra.edge.Edge)
(def Node edge_algebra.node.Node)


;; node roles with respect to an edge:
(def node-roles [
                 :right-face
                 :dest-vertex
                 :left-face
                 :origin-vertex
                 ])

(defn node-role
  "The role of node with respect to edge"
  [node edge]
  (let [role-index (mod (- (.-r node) (.-r edge)) 4)]
    (node-roles role-index)))


(defmulti dual
  "The dual of an Edge or a Node is its counterpart in the dual subdivision."
  class)


(defmethod dual Edge
  [edge]
  (rot (flip edge)))

(defmethod dual Node
  [node]
  (let [an-edge (get-edge (.getEdgeRecord node) 0 0)
        node-relationship-to-edge (node-role node an-edge)
        dual-edge (dual an-edge)
        dual-node-relationship-to-dual-edge (condp = node-relationship-to-edge
                                              :right-face (if (zero? (.-f node))
                                                            dest-vertex
                                                            origin-vertex)

                                              :dest-vertex right-face

                                              :left-face (if (zero? (.-f node))
                                                          origin-vertex
                                                          dest-vertex)

                                              :origin-vertex left-face)]

    (dual-node-relationship-to-dual-edge dual-edge)))
