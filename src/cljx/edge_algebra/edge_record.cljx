(ns edge-algebra.edge-record
  (:require [edge-algebra.record :refer [edge-records add-edge-record!]]
            [edge-algebra.node :as n :refer [new-node!]]
            [edge-algebra.edge :as e :refer [new-edge!]]))


;; An EdgeRecord represents eight edges:
;; the four oriented and directed versions of an undirected edge and of its dual;
;; and the eight nodes that represent the vertices of the edge and of its dual.
;; The duals of vertices of an edge are faces incident to the edge's dual.

(defn new-edge-record!
  []
  (let [index (count @edge-records)

        n00 (new-node! 0 0)
        n01 (new-node! 0 1)
        nodes [
               [
                n00
                n01
                ]
               [
                (new-node! 1 0)
                (new-node! 1 1)
                ]
               [
                (new-node! 2 0 :clone-of n00)
                (new-node! 2 1 :clone-of n01)
                ]
               [
                (new-node! 3 0)
                (new-node! 3 1)
                ]
               ]

        rnext [0 3 2 1]

        edges (vec (for [r (range 0 4)]
                     (vec (for [f (range 0 2)]
                            (let [next {:r (rnext r) :f f :edge-record index}]
                              (new-edge! r f index next))))))]


    (doseq [orientation [0 1]]

      (doseq [rotation [0 1 2 3]]
        (#+clj .setEdgeRecord #+cljs n/setEdgeRecord (get-in nodes [rotation orientation]) index)))


    (let [er {:edges edges :nodes nodes}]
      (add-edge-record! er)
      er)))


















