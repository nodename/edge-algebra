(ns edge-algebra.edge-record
  (:require [edge-algebra.app-state :refer [next-er-index add-edge-record!]]
            [edge-algebra.node :refer [new-node!]]
            [edge-algebra.edge :refer [new-edge!]]))


;; An EdgeRecord represents eight edges:
;; the four oriented and directed versions of an undirected edge and of its dual;
;; and the eight nodes that represent the vertices of the edge and of its dual.
;; The duals of vertices of an edge are faces incident to the edge's dual.


(defn new-edge-record!
  []
  (let [index (next-er-index)

        n00 (new-node! 0 0 index)
        n01 (new-node! 0 1 index)

        nodes [
               [
                n00
                n01
                ]
               [
                (new-node! 1 0 index)
                (new-node! 1 1 index)
                ]
               [
                (new-node! 2 0 index :clone-of n00)
                (new-node! 2 1 index :clone-of n01)
                ]
               [
                (new-node! 3 0 index)
                (new-node! 3 1 index)
                ]
               ]

        rnext [0 3 2 1]

        edges (vec (for [r (range 0 4)]
                     (vec (for [f (range 0 2)]
                            (let [next {:r (rnext r) :f f :edge-record index}]
                              (new-edge! r f index next))))))]

    (let [er {:edges edges :nodes nodes}]
      (add-edge-record! er)
      er)))


















