(ns edge-algebra.edge-record
  (:require [edge-algebra.node :as n :refer [new-node!]]
            [edge-algebra.edge :as e :refer [new-edge!]]))


;; An EdgeRecord represents eight edges:
;; the four oriented and directed versions of an undirected edge and of its dual;
;; and the eight nodes that represent the vertices of the edge and of its dual.
;; The duals of vertices of an edge are faces incident to the edge's dual.


(defn new-edge-record!
  []
  (let [n00 (new-node! 0 0)
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

        edges (vec (for [r (range 0 4)]
                     (vec (for [f (range 0 2)]
                            (new-edge! r f)))))

        er {:nodes nodes :edges edges}]

    (doseq [orientation [0 1]]

      (doseq [rotation [0 1 2 3]]
        ;; back-pointers oh well:
        (#+clj .setEdgeRecord #+cljs e/setEdgeRecord (get-in edges [rotation orientation]) er)
        (#+clj .setEdgeRecord #+cljs n/setEdgeRecord (get-in nodes [rotation orientation]) er))

      (let [e0o (get-in edges [0 orientation])
            e1o (get-in edges [1 orientation])
            e2o (get-in edges [2 orientation])
            e3o (get-in edges [3 orientation])]
        (#+clj .setNext #+cljs e/setNext e0o e0o)
        (#+clj .setNext #+cljs e/setNext e1o e3o)
        (#+clj .setNext #+cljs e/setNext e2o e2o)
        (#+clj .setNext #+cljs e/setNext e3o e1o)))

    er))
















