(ns edge-algebra.node)

;; A Node represents either a vertex or a face of the graph


(defprotocol INode
  (getEdgeRecord [this])
  (setEdgeRecord [this er]))

(deftype Node [r ; rotation
               f ; flip or orientation
               unique-id
               #+clj ^:volatile-mutable edge-record ;; the containing edge-record
               #+cljs ^:mutable edge-record
               ]
  INode
  (getEdgeRecord [this]
    edge-record)

  (setEdgeRecord [this er]
    (set! edge-record er)
    this))


(defn new-node!
  [r f & {:keys [clone-of]
          :or {clone-of nil}}]
  (Node.
   r
   f
   (if (nil? clone-of)
     (gensym)
     (.-unique-id clone-of))
   nil))


(defn equal?
  "Are they the same node?"
  [node0 node1]
  (= (.-unique-id node0) (.-unique-id node1)))

