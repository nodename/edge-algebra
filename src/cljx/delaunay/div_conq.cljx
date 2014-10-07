(ns delaunay.div-conq
  (:require
   ;;
   ;; the two topological operators exported by the edge-algebra library:
   [edge-algebra.core :refer [make-edge! splice!]]
   ;;
   ;; application-specific mutators:
   [edge-algebra.state.app-mutators :refer [set-data! set-sym-data! remove-edge-record!]]
   ;;
   ;; some functions for navigating to related edges:
   [edge-algebra.edge :refer [sym o-next o-prev l-next r-prev]];;
   ;;
   ;; geometry support from toxi's geom library:
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec2]]
   [thi.ng.geom.core.matrix :refer [matrix44]]
   [thi.ng.geom.core.utils :refer [norm-sign2]]
   ;;
   ;; debugging aid:
   [edge-algebra.cheat-codes :refer [edge-info]]
   ;;
  ; [utils.spawn :refer [go-fn]]

   #+clj [clojure.core.async
          :refer [>! take! chan go go-loop alts!]]
   #+cljs [cljs.core.async
                    :refer [>! take! chan alts!]])
  #+cljs (:require-macros [cljs.core.async.macros
                           :refer [go]]))


;; An alias for the 2-D point constructor:
(def pt vec2)


;; For the Delaunay application, the data field of an Edge
;; will contain the coordinates of its origin:
(defn org
  [edge]
  (:data edge))

(defn set-org!
  [edge coords]
  (set-data! edge coords))

;; and similarly, the data field of its symmetric Edge
;; will contain the coordinates of its destination:
(defn dest
  [edge]
  (org (sym edge)))

(defn set-dest!
  [edge coords]
  (set-sym-data! edge coords))


(defn make-d-edge!
  [org dest]
  (-> (make-edge!)
      (set-org! org)
      (set-dest! dest)))


(defn connect!
  [a b]
  (let [e (make-d-edge! (dest a) (org b))]
    (splice! e (l-next a))
    (splice! (sym e) b)
    e))

(defn delete-edge!
  [e]
  (splice! e (o-prev e))
  (splice! (sym e) (o-prev (sym e)))
  (remove-edge-record! e))


;; In addition to the two topological operations,
;; we need just two geometric primitives: in-circle? and ccw?

(defn in-circle?
  "The predicate in-circle? is defined to be true if and only if
  point d is interior to the region of the plane that is bounded by the
  oriented circle abc and lies to the left of it."
  [a b c d]
  (let [matrix (matrix44 (.-x a) (.-y a) (#+clj .mag-squared #+cljs g/mag-squared a) 1
                         (.-x b) (.-y b) (#+clj .mag-squared #+cljs g/mag-squared b) 1
                         (.-x c) (.-y c) (#+clj .mag-squared #+cljs g/mag-squared c) 1
                         (.-x d) (.-y d) (#+clj .mag-squared #+cljs g/mag-squared d) 1)]
    (> #+clj (.determinant matrix)
       #+cljs (.call thi.ng.geom.core.matrix.Matrix44.prototype.thi$ng$geom$core$PDeterminant$determinant$arity$1 matrix)
       0)))


(defn ccw?
  "The predicate ccw? is true if the points a, b, and c
   form a counterclockwise-oriented triangle."
  [a b c]
  (pos? (norm-sign2 a b c)))

(defn right-of?
  [point edge]
  (ccw? point (dest edge) (org edge)))

(defn left-of?
  [point edge]
  (ccw? point (org edge) (dest edge)))







(defn sort-xy
  "Sort by x, and when xs are equal, sort by y"
  [seq]
  (sort-by (juxt #(.-x %) #(.-y %)) seq))

(defn halves
  [seq]
  (let [half-way (/ (count seq) 2)]
    (map vec (split-at half-way seq))))


(defn slide-left!
  [edge]
  (let [t (o-next edge)]
    (delete-edge! edge)
    t))

(defn slide-right!
  [edge]
  (let [t (o-prev edge)]
    (delete-edge! edge)
    t))


(defn dest-above?
  "Is the dest point of edge above this right-to-left cross-edge?"
  [edge cross-edge]
  (right-of? (dest edge) cross-edge))


(defn bubble-left!
  "Locate the first l point (dest l-candidate) to be encountered by the rising bubble,
   and delete any l edges coming out of (dest cross-edge) that fail the circle test.
   Return the left candidate edge."
  [cross-edge]
  (let [initial-edge (o-next (sym cross-edge))]
    (if (dest-above? initial-edge cross-edge)
      (loop [edge initial-edge]
        (if (in-circle? (dest cross-edge) (org cross-edge) (dest edge)
                        (dest (o-next edge)))
          (recur (slide-left! edge))
          edge))
      initial-edge)))

(defn bubble-right!
  "Symmetrically to bubble-left!, return the right candidate edge."
  [cross-edge]
  (let [initial-edge (o-prev cross-edge)]
    (if (dest-above? initial-edge cross-edge)
      (loop [edge initial-edge]
        (if (in-circle? (dest cross-edge) (org cross-edge) (dest edge)
                        (dest (o-prev edge)))
          (recur (slide-right! edge))
          edge))
      initial-edge)))

(defn lower-common-tangent
  [ldi rdi]
  (loop [left ldi
         right rdi]
    (cond
     (left-of? (org right) left)  (recur (l-next left) right)
     (right-of? (org left) right) (recur left (r-prev right))
     :else [left right])))


(defn bubble-up!
  [initial-cross-edge]
  (loop [cross-edge initial-cross-edge]
    (let [l-candidate (bubble-left! cross-edge)
          r-candidate (bubble-right! cross-edge)
          ;;
          dest-above-cross-edge? (fn [edge] (dest-above? edge cross-edge))]
      ;;
      ;; If neither (dest l-candidate) nor (dest r-candidate) is above cross-edge,
      ;; then cross-edge is the upper common tangent and we're done.
      ;;
      ;; Otherwise:
      (when (or (dest-above-cross-edge? l-candidate)
                (dest-above-cross-edge? r-candidate))
        ;;
        ;; The next cross edge is to be connected to either
        ;; (dest l-candidate) or (dest r-candidate).
        ;; If both dests are above cross-edge,
        ;; then choose the appropriate one using the in-circle? test:
        (if (or (not (dest-above-cross-edge? l-candidate))
                (and (dest-above-cross-edge? r-candidate)
                     (in-circle? (dest l-candidate) (org l-candidate) (org r-candidate)
                                 (dest r-candidate))))
          ;;
          ;; Add new cross edge from (dest r-candidate) to (dest cross-edge):
          (recur (connect! r-candidate (sym cross-edge)))
          ;;
          ;; Else add new cross edge from (org cross-edge) to (dest l-candidate):
          (recur (connect! (sym cross-edge) (sym l-candidate))))))))


(defn merge!
  [ldo ldi rdi rdo]
  ;;
  ;; Compute the lower common tangent of l and r:
  (let [[left-tangent right-tangent] (lower-common-tangent ldi rdi)
        ;;
        ;; Create initial-cross-edge from (org right-tangent) to (org left-tangent)
        ;; (Note that we always choose the right-to-left direction for a cross-edge):
        initial-cross-edge (connect! (sym right-tangent) left-tangent)]

    (bubble-up! initial-cross-edge)

    ;; Return two edges: the counterclockwise convex-hull edge out of the leftmost vertex,
    ;; and the clockwise convex-hull edge out of the rightmost vertex.
    (let [left-edge (if (= (org left-tangent) (org ldo))
                      (sym initial-cross-edge)
                      ldo)
          right-edge (if (= (org right-tangent) (org rdo))
                       initial-cross-edge
                       rdo)]
      [left-edge right-edge])))


(defn delaunay-2'
  [sites]
  (let [[s1 s2] sites
        a (make-d-edge! s1 s2)]
    (println "2 returning" [(edge-info a) (edge-info (sym a))])
    [a (sym a)]))


(defn delaunay-3'
  [sites]
  (let [[s1 s2 s3] sites
        a (make-d-edge! s1 s2)
        b (make-d-edge! s2 s3)]
    (splice! (sym a) b)
    ;; Now close the triangle:
    (cond
     (ccw? s1 s2 s3) (do
                       (connect! b a)
                       [a (sym b)])
     (ccw? s1 s3 s2) (let [c (connect! b a)]
                       [(sym c) c])
     ;; Otherwise the three points are collinear:
     :else [a (sym b)])))


(defn delaunay'
  "Calculate the Delaunay triangulation of the sites; assume the sites are sorted."
  [sites]
  (println "delaunay' sites:" sites)
  (condp = (count sites)
      2 (delaunay-2' sites)
      3 (delaunay-3' sites)
      ;;
      ;; The default case, four or more sites: divide and conquer
      (let [; godel (go-fn delaunay')
            [l r] (halves sites)]
        (println)
        (println "r:" r)
        (println "l:" l)
        (println)
        #_(let [l-chan (godel l)
              r-chan (godel r)]
           (go
            (let [[ldo ldi] (<! l-chan)
                  [rdi rdo] (<! r-chan)]
              (merge! ldo ldi rdi rdo))))

              (let [[ldo ldi] (delaunay' l)
                    [rdi rdo] (delaunay' r)]
                (merge! ldo ldi rdi rdo))

        )))


(defn delaunay
  "Run delaunay' on sorted sites with no duplicates.
  This makes all future splittings constant-time operations."
  [sites]
  (delaunay' (sort-xy (vec (distinct sites)))))
