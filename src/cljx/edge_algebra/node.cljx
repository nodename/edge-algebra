(ns edge-algebra.node
  (:require [edge-algebra.app-state :refer [get-edge-record]]))

;; A Node represents either a vertex or a face of the graph

(defn new-node!
  [r f edge-record-index & {:keys [clone-of]
                            :or {clone-of nil}}]
  {:r r
   :f f
   :edge-record edge-record-index
   :unique-id (if (nil? clone-of)
               (gensym)
               (:unique-id clone-of))})


(defn equal?
  "Are they the same node?"
  [node0 node1]
  (= (:unique-id node0) (:unique-id node1)))

