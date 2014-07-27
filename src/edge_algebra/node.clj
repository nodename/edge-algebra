(ns edge-algebra.node)

;; A Node represents either a vertex or a face of the graph


(definterface INode
  (getEdgeRecord [])
  (setEdgeRecord [er]))

(deftype Node [r ; rotation
               f ; flip or orientation
               unique-node
               ^:volatile-mutable edge-record] ;; the containing edge-record, which holds eight Edges
  INode
  (getEdgeRecord [this]
    edge-record)

  (setEdgeRecord [this er]
    (set! edge-record er)
    this))


(defn make-node!
  [r f & {:keys [clone-of]
          :or {clone-of nil}}]
  (->Node
   r
   f
   (if (nil? clone-of)
     (gensym)
     (.-unique-node clone-of))
   nil))


(defn equal?
  "Are they the same node?"
  [node0 node1]
  (= (.-unique-node node0) (.-unique-node node1)))


(defn get-node
  [edge-record rotation f]
  (get-in edge-record [:nodes (mod rotation 4) (mod f 2)]))
