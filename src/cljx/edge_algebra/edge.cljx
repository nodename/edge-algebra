(ns edge-algebra.edge
  (:require [edge-algebra.record :refer [get-node get-edge]]))


;; Edges need a mutable next field so we make it a type with a volatile field.
;; http://macromancy.com/2014/01/16/data-structures-clojure-singly-linked-list.html

(defprotocol IEdge
  (getNext [this])
  (setNext [this e])
  (getEdgeRecord [this])
  (setEdgeRecord [this er])
  (getData [this])
  (setData [this data]))

(declare sym)

(deftype Edge [r ; rotation
               f ; flip or orientation
               #+clj ^:volatile-mutable next
               #+cljs ^:mutable next
               #+clj ^:volatile-mutable edge-record ; the containing edge-record
               #+cljs ^:mutable edge-record ; the containing edge-record
               #+clj ^:volatile-mutable data
               #+cljs ^:mutable data
               ]
  IEdge
  (getNext [this]
           next)

  (setNext [this e]
           (set! next e)
           this)

  (getEdgeRecord [this]
                 edge-record)

  (setEdgeRecord [this er]
                 (set! edge-record er)
                 this)

  (getData [this]
           data)

  (setData [this d]
           (set! data d)
           this)

  Object
  (toString [this]
            (str r " " f " " data "->" (.getData (sym this)))))



(defn new-edge!
  [r f]
  (Edge. r f nil nil nil))



;; The four nodes directly connected to this edge
;; represent its direction and orientation:

;; ## Direction: origin-vertex and dest-vertex

(defn origin-vertex
  [edge]
  (get-node (.getEdgeRecord edge) (+ (.r edge) 3) (.f edge)))

(defn dest-vertex
  [edge]
  (get-node (.getEdgeRecord edge) (+ (.r edge) 1) (.f edge)))

;; ## Orientation: left-face and right-face

(defn left-face
  [edge]
  (let [r (.-r edge)
        f (.-f edge)]
    (get-node (.getEdgeRecord edge) (+ (+ r 2) (* 2 f)) f)))

(defn right-face
  [edge]
  (let [r (.-r edge)
        f (.-f edge)]
    (get-node (.getEdgeRecord edge) (+ r (* 2 f)) f)))


;; get the three related edges within the same edge-record: rot, sym, and flip

(defn rot
  ([edge] (rot 1 edge))
  ([exponent edge]
   (let [r (.-r edge)
         f (.-f edge)]
     (get-edge (.getEdgeRecord edge) (+ r (* (+ 1 (* 2 f)) exponent)) f))))

(defn sym
  "return the symmetric QuadEdge: the one with same orientation and opposite direction"
  ([edge] (sym 1 edge))
  ([exponent edge]
   (let [r (.-r edge)
         f (.-f edge)]
     (get-edge (.getEdgeRecord edge) (+ r (* 2 exponent)) f))))

(defn flip
  "return the QuadEdge with same direction and opposite orientation"
  ([edge] (flip 1 edge))
  ([exponent edge]
   (let [r (.-r edge)
         f (.-f edge)]
     (get-edge (.getEdgeRecord edge) r (+ f exponent)))))


;; get connected edges: oPrev. oNext, dPrev, dNext, lPrev, lNext, rPrev, rNext

;; First we give the eight primitive operations
;; that find the next and prev edges through
;; each of the four rings in which this edge participates.
;; Note that they all access a single property of
;; the Edge type, .getNext:

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges out of originVertex:
(defn onext [edge] (.getNext edge))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges out of originVertex:
(def oprev (comp rot onext rot))

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges into destVertex:
(def dnext (comp sym onext sym))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges into destVertex:
(def dprev (comp #(rot -1 %) onext #(rot -1 %)))

;; find the next counterclockwise QuadEdge with the same left face:
(def lnext (comp rot onext #(rot -1 %)))

;; find the next clockwise QuadEdge with the same left face:
(def lprev (comp sym onext))

;; find the next clockwise QuadEdge with the same right face:
(def rnext (comp #(rot -1 %) onext rot))

;; find the next clockwise QuadEdge with the same right face:
(def rprev (comp onext sym))

;; Now we develop the interfaces to these operations
;; that allow an exponent indicating how many steps (positive or negative)
;; to traverse the respective rings:

(def invert {:next :prev
             :prev :next})

(def ops
  {:origin {:next onext :prev oprev}
   :dest   {:next dnext :prev dprev}
   :left   {:next lnext :prev lprev}
   :right  {:next rnext :prev rprev}})


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
  (let [[op exponent] (rectify link direction exponent)]
    (nth (iterate op edge) exponent)))


(defn o-next
  ([edge] (o-next 1 edge))
  ([exponent edge] (neighbor edge :origin :next exponent)))

(defn o-prev
  ([edge] (o-prev 1 edge))
  ([exponent edge] (neighbor edge :origin :prev exponent)))

(defn d-next
  ([edge] (d-next 1 edge))
  ([exponent edge] (neighbor edge :dest :next exponent)))

(defn d-prev
  ([edge] (d-prev 1 edge))
  ([exponent edge] (neighbor edge :dest :prev exponent)))

(defn l-next
  ([edge] (l-next 1 edge))
  ([exponent edge] (neighbor edge :left :next exponent)))

(defn l-prev
  ([edge] (l-prev 1 edge))
  ([exponent edge] (neighbor edge :left :prev exponent)))

(defn r-next
  ([edge] (r-next 1 edge))
  ([exponent edge] (neighbor edge :right :next exponent)))

(defn r-prev
  ([edge] (r-prev 1 edge))
  ([exponent edge] (neighbor edge :right :prev exponent)))


