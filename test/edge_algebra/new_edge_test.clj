(ns edge-algebra.new-edge-test
  (:require [clojure.test :refer :all]
            [edge-algebra.core :refer [new-edge]]
            [edge-algebra.edge :refer :all]
            [edge-algebra.node :refer [equal?]]))

;; These tests must pass for any newly-created edge

(declare ^:dynamic *new-edge*)
(declare ^:dynamic *new-edge-rot*)

(defn wrap-edge-creation
  [test-fn]
  (let [new-edge (new-edge)
        new-edge-rot (rot new-edge)]
    (binding [*new-edge* new-edge
              *new-edge-rot* new-edge-rot]
      (test-fn))))

(use-fixtures :each wrap-edge-creation)


;; edge tests:


(deftest newEdge-lnext-is-rnext
  (is (identical?
       (l-next *new-edge*)
       (r-next *new-edge*))))

(deftest newEdge-lnext-is-sym
  (is (identical?
       (l-next *new-edge*)
       (sym *new-edge*))))

(deftest newEdge-onext-is-oprev
  (is (identical?
       (o-next *new-edge*)
       (o-prev *new-edge*))))

(deftest newEdge-onext-is-edge
  (is (identical?
       (o-next *new-edge*)
       *new-edge*)))


(deftest newEdgeRot-lnext-is-rnext
  (is (identical?
       (l-next *new-edge-rot*)
       (r-next *new-edge-rot*))))

(deftest newEdgeRot-lnext-is-edge
  (is (identical?
       (l-next *new-edge-rot*)
       *new-edge-rot*)))

(deftest newEdgeRot-onext-is-oprev
  (is (identical?
       (o-next *new-edge-rot*)
       (o-prev *new-edge-rot*))))

(deftest newEdgeRot-onext-is-sym
  (is (identical?
       (o-next *new-edge-rot*)
       (sym *new-edge-rot*))))



;; node tests:


(deftest newEdge-origin-vertex-differs-from-dest-vertex
  (is (not (equal?
            (origin-vertex *new-edge*)
            (dest-vertex *new-edge*)))))

(deftest newEdge-left-face-equals-right-face
  (is (equal?
       (left-face *new-edge*)
       (right-face *new-edge*))))


(deftest newEdgeRot-origin-vertex-equals-dest-vertex
  (is (equal?
       (origin-vertex *new-edge-rot*)
       (dest-vertex *new-edge-rot*))))

(deftest newEdgeRot-left-face-differs-from-right-face
  (is (not (equal?
            (left-face *new-edge-rot*)
            (right-face *new-edge-rot*)))))









