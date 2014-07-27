(ns edge-algebra.edge
  (:require [edge-algebra.node :refer [get-node]]))


;; Edges need a mutable o-next-ref field so we make it a type with a volatile field.
;; http://macromancy.com/2014/01/16/data-structures-clojure-singly-linked-list.html

(definterface IEdge
  (getNextRef [])
  (setNextRef [e])
  (getEdgeRecord [])
  (setEdgeRecord [er])
  (getData [])
  (setData [data]))

(deftype Edge [r ; rotation
               f ; flip or orientation
               ^:volatile-mutable o-next-ref
               ^:volatile-mutable edge-record ; the containing edge-record, which holds eight Edges
               ^:volatile-mutable data]
  IEdge
  (getNextRef [this]
   o-next-ref)

  (setNextRef [this e]
   (set! o-next-ref e)
   this)

  (getEdgeRecord [this]
    edge-record)

  (setEdgeRecord [this er]
    (set! edge-record er)
    this))


(defn make-edge!
  [r f]
  (->Edge r f nil nil nil))


(defn get-edge
  [edge-record rotation f]
  (get-in edge-record [:edges (mod rotation 4) (mod f 2)]))


;; The four nodes directly connected to this edge
;; represent its direction and orientation:

;; direction: origin-vertex and dest-vertex

(defn origin-vertex
  [edge]
  (get-node (.getEdgeRecord edge) (+ (.r edge) 3) (.f edge)))

(defn dest-vertex
  [edge]
  (get-node (.getEdgeRecord edge) (+ (.r edge) 1) (.f edge)))

;; orientation: left-face and right-face

(defn left-face
  [edge]
  (let [r (.r edge)
        f (.f edge)]
    (get-node (.getEdgeRecord edge) (+ (+ r 2) (* 2 f)) f)))

(defn right-face
  [edge]
  (let [r (.r edge)
        f (.f edge)]
    (get-node (.getEdgeRecord edge) (+ r (* 2 f)) f)))


;; get the three related edges within the same edge-record: rot, sym, and flip

(defn rot
  ([edge] (rot edge 1))
  ([edge exponent]
   (let [r (.r edge)
         f (.f edge)]
     (get-edge (.getEdgeRecord edge) (+ r (* (+ 1 (* 2 f)) exponent)) f))))

(defn sym
  "return the symmetric QuadEdge: the one with same orientation and opposite direction"
  ([edge] (sym edge 1))
  ([edge exponent]
   (let [r (.r edge)
         f (.f edge)]
     (get-edge (.getEdgeRecord edge) (+ r (* 2 exponent)) f))))

(defn flip
  "return the QuadEdge with same direction and opposite orientation"
  ([edge] (flip edge 1))
  ([edge exponent]
   (let [r (.r edge)
         f (.f edge)]
     (get-edge (.getEdgeRecord edge) r (+ f exponent)))))


;; get connected edges: oPrev. oNext, dPrev, dNext, lPrev, lNext, rPrev, rNext

;; First we give the eight primitive operations
;; that find the next and prev edges through
;; each of the four rings in which this edge participates.
;; Note that they all access a single property of
;; the Edge type, o-next-ref:

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges out of originVertex:
(defn onext [edge] (.getNextRef edge))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges out of originVertex:
(def oprev (comp rot onext rot))

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges into destVertex:
(def dnext (comp sym onext sym))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges into destVertex:
(def dprev (comp #(rot % -1) onext #(rot % -1)))

;; find the next counterclockwise QuadEdge with the same left face:
(def lnext (comp rot onext #(rot % -1)))

;; find the next clockwise QuadEdge with the same left face:
(def lprev (comp sym onext))

;; find the next clockwise QuadEdge with the same right face:
(def rnext (comp #(rot % -1) onext rot))

;; find the next clockwise QuadEdge with the same right face:
(def rprev (comp onext sym))

;; Now we develop the interfaces to these operations
;; that allow an exponent indicating how many steps (positive or negative)
;; to traverse the respective rings:

(defn invert
  [direction]
  (condp = direction
    :pos :neg
    :neg :pos))

(def ops
  {:origin {:pos onext :neg oprev}
   :dest {:pos dnext :neg dprev}
   :left {:pos lnext :neg lprev}
   :right {:pos rnext :neg rprev}})


(defn rectify
  "Get the correct op (invert direction and exponent if exponent is negative)"
  [link direction exponent]
  (let [[direction exponent] (if (neg? exponent)
                               [(invert direction) (- exponent)]
                               [direction exponent])
        op (get-in ops [link direction])]

    [op exponent]))


(defn neighbor
  [edge link direction exponent]
  (let [exponent (if (nil? exponent) 1 exponent)
        [op exponent] (rectify link direction exponent)]
    (nth (iterate op edge) exponent)))



(defn o-next [edge & [exponent]]
  (neighbor edge :origin :pos exponent))

(defn o-prev [edge & [exponent]]
  (neighbor edge :origin :neg exponent))

(defn d-next [edge & [exponent]]
  (neighbor edge :dest :pos exponent))

(defn d-prev [edge & [exponent]]
  (neighbor edge :dest :neg exponent))

(defn l-next [edge & [exponent]]
  (neighbor edge :left :pos exponent))

(defn l-prev [edge & [exponent]]
  (neighbor edge :left :neg exponent))

(defn r-next [edge & [exponent]]
  (neighbor edge :right :pos exponent))

(defn r-prev [edge & [exponent]]
  (neighbor edge :right :neg exponent))


