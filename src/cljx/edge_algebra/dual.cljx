(ns edge-algebra.dual
  (:require [edge-algebra.app-state :refer [get-edge-record]]
            [edge-algebra.record :refer [get-e0]]
            [edge-algebra.edge :refer [rot flip
                                       origin-vertex dest-vertex
                                       left-face right-face]]))

;; Node roles with respect to an edge:
(def node-roles [
                 :right-face
                 :dest-vertex
                 :left-face
                 :origin-vertex
                 ])

(defn node-role
  "The role of node with respect to edge."
  [node edge]
  (let [role-index (mod (- (:r node) (:r edge)) 4)]
    (node-roles role-index)))


(defn which
  [node-or-edge]
  (if (nil? (:next node-or-edge))
    :node
    :edge))

(defmulti dual
  "The dual of an Edge or a Node is its counterpart in the dual subdivision."
  which)


(defmethod dual :edge
  [edge]
  (rot (flip edge)))

(defmethod dual :node
  [node]
  (let [e0 (get-e0 (get-edge-record node))
        node-relationship-to-e0 (node-role node e0)
        dual-edge (dual e0)
        dual-node-relationship-to-dual-edge (condp = node-relationship-to-e0
                                              :right-face (if (zero? (:f node))
                                                            dest-vertex
                                                            origin-vertex)

                                              :dest-vertex right-face

                                              :left-face (if (zero? (:f node))
                                                          origin-vertex
                                                          dest-vertex)

                                              :origin-vertex left-face)]

    (dual-node-relationship-to-dual-edge dual-edge)))
