(ns edge-algebra.edge-test
  (:require [clojure.test :refer :all]
            [edge-algebra.core :refer [new-edge dual]]
            [edge-algebra.edge :refer :all]
            [edge-algebra.node :refer [equal?]]))

;; These tests must pass for any edge at any time,
;; but let's test them on a newly-created edge.

(declare ^:dynamic *edge*)

(defn wrap-edge-creation
  [test-fn]
  (let [new-edge (new-edge)]
    (binding [*edge* new-edge]
      (test-fn))))

(use-fixtures :each wrap-edge-creation)


(deftest dual-definition-equation-1
  (is (identical?
       (dual (dual *edge*))
       *edge*)))

(deftest dual-definition-equation-2
  (is (identical?
       (dual (sym *edge*))
       (sym (dual *edge*)))))

(deftest dual-definition-equation-3
  (is (identical?
       (dual (flip *edge*))
       (sym (flip (dual *edge*))))))

(deftest dual-definition-equation-4
  (is (identical?
       (dual (l-next *edge*))
       (o-next (dual *edge*) -1))))

(deftest dual-definition-face-to-vertex
  (is (identical?
       (dual (left-face *edge*))
       (origin-vertex (dual *edge*)))))

(deftest dual-definition-vertex-to-face
  (is (identical?
       (dual (origin-vertex *edge*))
       (left-face (dual *edge*)))))

(deftest rot-is-dual-flip
  (is (identical?
       (rot *edge*)
       (dual (flip *edge*)))))

(deftest rot-rot-is-sym
  (is (identical?
       (rot (rot *edge*))
       (sym *edge*))))

(deftest edge-functions-property-1
  (is (identical?
       (rot *edge* 4)
       *edge*)))

(deftest edge-functions-property-2
  (is (identical?
       (o-next (rot (o-next (rot *edge*))))
       *edge*)))

(deftest edge-functions-property-3
  (is (not (identical?
            (rot *edge* 2)
            *edge*))))

#_
(deftest edge-functions-property-4
  ;; if Subdivisions s and s' are duals, then s contains edge e iff s' contains e.dual
  )

#_
(deftest edge-functions-property-5
  ;; Subdivision s contains edge e iff s contains e.onext
  )

(deftest flip-functions-property-1
  (is (identical?
       (flip *edge* 2)
       *edge*)))

(deftest flip-functions-property-2
  (is (identical?
       (o-next (flip (o-next (flip *edge*))))
       *edge*)))

(defn property-3
  [n]
  (is (not (identical?
            (o-next (flip *edge*) n)
            *edge*))))

(deftest flip-functions-property-3
  ;; really for all n:
  (doseq [n (range -10 11)]
         (property-3 n)))

(deftest flip-functions-property-4
  (is (identical?
       (rot (flip (rot (flip *edge*))))
       *edge*)))

#_
(deftest flip-functions-property-5
  ;; Subdivision s contains edge e iff s contains e.flip
  )

(deftest useful-property-1
  (is (identical?
       (flip *edge* -1)
       (flip *edge*))))

(deftest useful-property-2
  (is (identical?
       (sym *edge*)
       (rot *edge* 2))))

(deftest useful-property-3
  (is (identical?
       (rot *edge* -1)
       (flip (rot (flip *edge*))))))

(deftest useful-property-4
  (is (identical?
       (rot *edge* -1)
       (flip (rot (flip *edge*))))))

(deftest useful-property-5
  (is (identical?
       (dual *edge*)
       (rot (flip *edge*)))))

(deftest useful-property-6
  (is (identical?
       (o-next *edge* -1)
       (rot (o-next (rot *edge*))))))

(deftest useful-property-7
  (is (identical?
       (o-next *edge* -1)
       (flip (o-next (flip *edge*))))))

(deftest useful-property-8
  (is (identical?
       (l-next *edge*)
       (rot (o-next (rot *edge* -1))))))

(deftest useful-property-9
  (is (identical?
       (r-next *edge*)
       (rot (o-next (rot *edge*)) -1))))

(deftest useful-property-10
  (is (identical?
       (d-next *edge*)
       (sym (o-next (sym *edge*))))))

(deftest left-definition
  )
