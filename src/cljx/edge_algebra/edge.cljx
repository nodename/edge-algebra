(ns edge-algebra.edge
  (:require [edge-algebra.record :refer [IType get-node get-edge]]))

(defprotocol IEdge
  (getNext [this])
  (setNext [this e])
  (getEdgeRecord [this])
  (setEdgeRecord [this er])
  (getData [this])
  (setData [this d]))


(declare sym)

(deftype Edge [r ; rotation
               f ; flip or orientation
               #+clj ^:volatile-mutable data #+cljs ^:mutable data
               #+clj ^:volatile-mutable edge-record #+cljs ^:mutable edge-record
               #+clj ^:volatile-mutable next #+cljs ^:mutable next
               ]
  IEdge
  (getNext [this]
           next)

  (setNext [this e]
           #+clj (set! next e)
           #+cljs (aset this "next" e)
           this)

  (getEdgeRecord [this]
                 edge-record)

  (setEdgeRecord [this er]
                 #+clj (set! edge-record er)
                 #+cljs (aset this "edge_record" er) ;; underscore? pwetty twicky!
                 this)

  (getData [this]
           data)

  (setData [this d]
           #+clj (set! data d)
           #+cljs (aset this "data" d)
           this)

  IType
  (getType [this]
            :edge)

  Object
  ;; This one is a bit smelly because it implies knowledge
  ;; of what an application might store in the data fields:
  (toString [this]
            (str r " " f " " data
            "->" #+clj (.getData (sym this) #+cljs (.-data (sym this))))))


(defn new-edge!
  [r f]
  (Edge. r f nil nil nil))



(defn get-edge-record
  [edge]
  #+clj (.getEdgeRecord edge)
  #+cljs (getEdgeRecord edge))



;; The four nodes directly connected to this edge
;; represent its direction and orientation:

;; ## Direction: origin-vertex and dest-vertex

(defn origin-vertex
  [edge]
  (let [r (+ (.-r edge) 3)
        f (.-f edge)]
    (get-node (get-edge-record edge) r f)))

(defn dest-vertex
  [edge]
  (let [r (+ (.-r edge) 1)
        f (.-f edge)]
    (get-node (get-edge-record edge) r f)))

;; ## Orientation: left-face and right-face

(defn left-face
  [edge]
  (let [r (+ (+ (.-r edge) 2) (* 2 (.-f edge)))
        f (.-f edge)]
    (get-node (get-edge-record edge) r f)))

(defn right-face
  [edge]
  (let [r (+ (.-r edge) (* 2 (.-f edge)))
        f (.-f edge)]
    (get-node (get-edge-record edge) r f)))


;; get the three related edges within the same edge-record: rot, sym, and flip

(defn rot
  ([edge] (rot 1 edge))
  ([exponent edge]
   (let [r (+ (.-r edge) (* (+ 1 (* 2 (.-f edge))) exponent))
         f (.-f edge)]
     (get-edge (get-edge-record edge) r f))))

(defn sym
  "return the symmetric QuadEdge: the one with same orientation and opposite direction"
  ([edge] (sym 1 edge))
  ([exponent edge]
   (let [r (+ (.-r edge) (* 2 exponent))
         f (.-f edge)]
     (get-edge (get-edge-record edge) r f))))

(defn flip
  "return the QuadEdge with same direction and opposite orientation"
  ([edge] (flip 1 edge))
  ([exponent edge]
   (let [r (.-r edge)
         f (+ (.-f edge) exponent)]
     (get-edge (get-edge-record edge) r f))))


;; get connected edges: oPrev. oNext, dPrev, dNext, lPrev, lNext, rPrev, rNext

;; First we give the eight primitive operations
;; that find the next and prev edges through
;; each of the four rings in which this edge participates.
;; Note that they all access a single property of
;; the Edge type, .getNext:

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges out of originVertex:
(defn ^:private onext [edge] (#+clj .getNext #+cljs getNext edge))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges out of originVertex:
(def ^:private oprev (comp rot onext rot))

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges into destVertex:
(def ^:private dnext (comp sym onext sym))

;; find the QuadEdge immediately following this one
;; clockwise in the ring of edges into destVertex:
(def ^:private dprev (comp #(rot -1 %) onext #(rot -1 %)))

;; find the next counterclockwise QuadEdge with the same left face:
(def ^:private lnext (comp rot onext #(rot -1 %)))

;; find the next clockwise QuadEdge with the same left face:
(def ^:private lprev (comp sym onext))

;; find the next clockwise QuadEdge with the same right face:
(def ^:private rnext (comp #(rot -1 %) onext rot))

;; find the next clockwise QuadEdge with the same right face:
(def ^:private rprev (comp onext sym))

;; Now we develop the interfaces to these operations
;; that allow an exponent indicating how many steps (positive or negative)
;; to traverse the respective rings:

(def invert {onext oprev
             oprev onext
             dnext dprev
             dprev dnext
             lnext lprev
             lprev lnext
             rnext rprev
             rprev rnext})


(defn neighbor
  [edge op exponent]
  (let [[op exponent] (if (neg? exponent)
                        [(invert op) (- exponent)]
                        [op exponent])]
    (nth (iterate op edge) exponent)))


(defn o-next
  ([edge] (o-next 1 edge))
  ([exponent edge] (neighbor edge onext exponent)))

(defn o-prev
  ([edge] (o-prev 1 edge))
  ([exponent edge] (neighbor edge oprev exponent)))

(defn d-next
  ([edge] (d-next 1 edge))
  ([exponent edge] (neighbor edge dnext exponent)))

(defn d-prev
  ([edge] (d-prev 1 edge))
  ([exponent edge] (neighbor edge dprev exponent)))

(defn l-next
  ([edge] (l-next 1 edge))
  ([exponent edge] (neighbor edge lnext exponent)))

(defn l-prev
  ([edge] (l-prev 1 edge))
  ([exponent edge] (neighbor edge lprev exponent)))

(defn r-next
  ([edge] (r-next 1 edge))
  ([exponent edge] (neighbor edge rnext exponent)))

(defn r-prev
  ([edge] (r-prev 1 edge))
  ([exponent edge] (neighbor edge rprev exponent)))



