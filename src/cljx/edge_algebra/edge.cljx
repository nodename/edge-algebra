(ns edge-algebra.edge
  (:require [edge-algebra.app-state :refer [ get-edge-record]]
            [edge-algebra.record :refer [get-node get-edge]]))


(defn new-edge!
  [r f edge-record-index next]
  {:r r
   :f f
   :data nil
   :edge-record edge-record-index
   :next next})


;; The four nodes directly connected to this edge
;; represent its direction and orientation:

;; ## Direction: origin-vertex and dest-vertex

(defn origin-vertex
  [edge]
  (let [r (+ (:r edge) 3)
        f (:f edge)]
    (get-node (get-edge-record edge) r f)))

(defn dest-vertex
  [edge]
  (let [r (+ (:r edge) 1)
        f (:f edge)]
    (get-node (get-edge-record edge) r f)))

;; ## Orientation: left-face and right-face

(defn left-face
  [edge]
  (let [r (+ (+ (:r edge) 2) (* 2 (:f edge)))
        f (:f edge)]
    (get-node (get-edge-record edge) r f)))

(defn right-face
  [edge]
  (let [r (+ (:r edge) (* 2 (:f edge)))
        f (:f edge)]
    (get-node (get-edge-record edge) r f)))


;; get the three related edges within the same edge-record: rot, sym, and flip

(defn rot
  ([edge] (rot 1 edge))
  ([exponent edge]
   (let [r (+ (:r edge) (* (+ 1 (* 2 (:f edge))) exponent))
         f (:f edge)]
     (get-edge (get-edge-record edge) r f))))

(defn sym
  "return the symmetric QuadEdge: the one with same orientation and opposite direction"
  ([edge] (sym 1 edge))
  ([exponent edge]
  ; (println "SYM: input r f: " (:r edge) " " (:f edge))
   (let [r (+ (:r edge) (* 2 exponent))
         f (:f edge)]
   ;  (println "SYM: new r f: " r " " f)
     (get-edge (get-edge-record edge) r f))))

(defn flip
  "return the QuadEdge with same direction and opposite orientation"
  ([edge] (flip 1 edge))
  ([exponent edge]
   (let [r (:r edge)
         f (+ (:f edge) exponent)]
     (get-edge (get-edge-record edge) r f))))


;; get connected edges: oPrev. oNext, dPrev, dNext, lPrev, lNext, rPrev, rNext

;; First we give the eight primitive operations
;; that find the next and prev edges through
;; each of the four rings in which this edge participates.
;; Note that they all access a single property of
;; the Edge type, .getNext:

;; find the QuadEdge immediately following this one
;; counterclockwise in the ring of edges out of originVertex:
(defn ^:private onext [edge]
  (get-edge (get-edge-record (:next edge))
                                       (:r (:next edge)) (:f (:next edge))))

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



