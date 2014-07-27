(ns edge-algebra.edge-record
  (:require [edge-algebra.node :refer [make-node!]]
            [edge-algebra.edge :refer [make-edge!]]))


;; An EdgeRecord represents eight edges:
;; the four oriented and directed versions of an undirected edge and of its dual;
;; and the eight nodes that represent the vertices of the edge and of its dual.
;; The duals of vertices of an edge are faces incident to the edge's dual.


(defn make-edge-record!
  []
  (let [n00 (make-node! 0 0)
        n01 (make-node! 0 1)
        nodes [
               [
                n00
                n01
                ]
               [
                (make-node! 1 0)
                (make-node! 1 1)
                ]
               [
                (make-node! 2 0 :clone-of n00)
                (make-node! 2 1 :clone-of n01)
                ]
               [
                (make-node! 3 0)
                (make-node! 3 1)
                ]
               ]

        edges (vec (for [r (range 0 4)]
                     (vec (for [f (range 0 2)]
                            (make-edge! r f)))))

        er {:nodes nodes :edges edges}]

    (doseq [orientation [0 1]]

      (doseq [rotation [0 1 2 3]]
        ;; back-pointers oh well:
        (.setEdgeRecord (get-in nodes [rotation orientation]) er)
        (.setEdgeRecord (get-in edges [rotation orientation]) er))

      (let [e0o (get-in edges [0 orientation])
            e1o (get-in edges [1 orientation])
            e2o (get-in edges [2 orientation])
            e3o (get-in edges [3 orientation])]
        (.setNextRef e0o e0o)
        (.setNextRef e1o e3o)
        (.setNextRef e2o e2o)
        (.setNextRef e3o e1o)))

    er))























