(ns edge-algebra.record)

;; This namespace holds some edge-record related vars
;; that we want to be able to require without introducing a dependency cycle.

;; Edge-record accessor functions:


(defn- get-elt
  [edge-record type r f]
  (get-in edge-record [type (mod r 4) (mod f 2)]))


(defn get-node
  [edge-record rotation f]
  (get-elt edge-record :nodes rotation f))


(defn get-edge
  [edge-record rotation f]
  (get-elt edge-record :edges rotation f))


(defn get-e0
  "Return the canonical representative edge of an edge record"
  [edge-record]
  (get-edge edge-record 0 0))

