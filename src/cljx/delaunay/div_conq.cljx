(ns delaunay.div-conq
  (:require
   ;;
   ;; the two topological operators exported by the edge-algebra library:
   [edge-algebra.core :refer [make-edge! splice!]]
   ;;
   ;; application-specific mutators:
   [edge-algebra.app-state :refer [set-data! set-sym-data! remove-edge-record!]]
   ;;
   ;; some functions for navigating to related edges:
   [edge-algebra.edge :as e :refer [sym o-next o-prev l-next r-prev]]
   ;;
   ;; geometry support from toxi's geom library:
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec2]]
   [thi.ng.geom.core.matrix :refer [matrix44]]
   [thi.ng.geom.core.utils :refer [norm-sign2]]))


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


(defn verts
  [edge]
  (str (org edge) "->" (dest edge)))



(defn make-d-edge!
  [org dest]
  (-> (make-edge!)
      (set-org! org)
      (set-dest! dest)))

(defn show-ring
  [f edge]
  (loop [e edge
         i 0]
    (when e
      (println e (verts e))
      (when-not (= i 10)
        (recur (f e) (inc i))))))


(defn connect!
  [a b]
  (println "connect!: connecting" (verts a) "-->>" (verts b))
  (let [e (make-d-edge! (dest a) (org b))]
    (println "connect: first splice" (verts e) (verts (l-next a)))
    (splice! e (l-next a))
    (println "connect: second splice" (verts (sym e)) (verts b))
    (splice! (sym e) b)
    (println "o-next sym e:" (o-next (sym e)) (verts (o-next (sym e))))
    #_(show-edge-records)
    e))

(defn delete-edge!
  [e]
  (println "delete-edge: Splicing" (verts e) (verts (o-prev e)))
  (splice! e (o-prev e))
  (println "delete-edge: Splicing" (verts (sym e)) (verts (o-prev (sym e))))
  (splice! (sym e) (o-prev (sym e)))
  (println "removing " (verts e))
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
  (let [result (ccw? point (dest edge) (org edge))]
    (println "right-of?" point (verts edge) result)
    result))

(defn left-of?
  [point edge]
  (let [result (ccw? point (org edge) (dest edge))]
    (println "left-of?" point (verts edge) result)
    result))


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
    (println "slide-left:" (verts t))
    (delete-edge! edge)
    t))

(defn slide-right!
  [edge]
  (let [t (o-prev edge)]
    (println "slide-right:" (verts t))
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
  (println "bubble-left entering cross-edge:" cross-edge (verts cross-edge))
  #_(println "sym:" (sym cross-edge) (verts (sym cross-edge)))
  #_(println "o-next:" (o-next (sym cross-edge)) (verts (o-next (sym cross-edge))))

  (println "bubble-left: (sym cross-edge)'s o-next ring:")
  (show-ring o-next (sym cross-edge))
  (let [initial-edge (o-next (sym cross-edge))]
    (println "bubble-left initial edge:" (verts initial-edge))
    (if (dest-above? initial-edge cross-edge)
      (do  (println "bubble-left: dest is above cross-edge")
      (loop [edge initial-edge]
        (println "bubble-left: edge:" (verts edge))
        (if (in-circle? (dest cross-edge) (org cross-edge) (dest edge)
                        (dest (o-next edge)))
          (recur (slide-left! edge))
          edge)))
      initial-edge)))

(defn bubble-right!
  "Symmetrically to bubble-left!, return the right candidate edge."
  [cross-edge]
  (println "bubble-right: cross-edge's o-next ring:")
  (show-ring o-next cross-edge)
  (let [initial-edge (o-prev cross-edge)]
    (println "bubble-right initial edge: " (verts initial-edge))
    (if (dest-above? initial-edge cross-edge)
      (do  (println "bubble-right: dest is above cross-edge")
      (loop [edge initial-edge]
        (println "bubble-right: edge:" (verts edge))
        (if (in-circle? (dest cross-edge) (org cross-edge) (dest edge)
                        (dest (o-prev edge)))
          (recur (slide-right! edge))
          edge)))
      initial-edge)))

(defn lower-common-tangent
  [ldi rdi]
  ;; This calculation is given incorrectly in the paper, with an ELSIF!
  ;; Or IS IT?
  (println "lower-common-tangent in: ldi:" (verts ldi) "rdi:" (verts rdi))
  (let [[ldi rdi] (cond
                   (left-of? (org rdi) ldi)  [(l-next ldi) rdi]
                   (right-of? (org ldi) rdi) [ldi (r-prev rdi)]
                   :else [ldi rdi])]
    (println "lower-common-tangent out: ldi:" (verts ldi) "rdi:" (verts rdi))
    [ldi rdi]))


(defn delaunay'
  "Calculate the Delaunay triangulation of the sites; return
   the counterclockwise convex hull edge out of the leftmost vertex
   and the clockwise convex hull edge out of the rightmost vertex.
  Assume the sites are sorted."
  [sites]
  (condp = (count sites)
      2 (let [[s1 s2] sites
              a (make-d-edge! s1 s2)]
          [a (sym a)])

      3 (let [[s1 s2 s3] sites
              a (make-d-edge! s1 s2)
              b (make-d-edge! s2 s3)]
          (println "triangle: Splicing" (verts (sym a)) (verts b))
          (splice! (sym a) b)
          ;; Now close the triangle:
          (cond
           (ccw? s1 s2 s3) (do
                             (connect! b a)
                             [a (sym b)])
           (ccw? s1 s3 s2) (do
                             (let [c (connect! b a)]
                               (println "triangle returning" (verts (sym c)) (verts c))
                               [(sym c) c]))
           ;; Otherwise the three points are collinear:
           :else [a (sym b)]))
      ;;
      ;; The default case, four or more sites: divide and conquer
      ;;
      (let [[l r] (halves sites)
            _ (println)
            _ (println "l:" l)
            _ (println "r:" r)
            [ldo ldi] (delaunay' l)
            [rdi rdo] (delaunay' r)
            _ (println "ldo:" (verts ldo) "ldi:" (verts ldi))
            _ (println "rdo:" (verts rdo) "rdi:" (verts rdi))
            ;; Compute the lower common tangent [ldi rdi] of l and r:
            [ldi rdi] (lower-common-tangent ldi rdi)
            _ (println "lower common tangent (ldi rdi):" (verts ldi) (verts rdi))
            ;;
            ;; Create initial-cross-edge from (org rdi) to (org ldi)
            ;; (Note that we always choose the right-to-left direction for a cross-edge):
            ;; Here's where we go wrong. ldi? How about (o-next ldi)? Nah
            initial-cross-edge (connect! (sym rdi) ldi)
            _ (println "initial cross edge:" (verts initial-cross-edge))
            ldo (if (= (org ldi) (org ldo))
                  (sym initial-cross-edge)
                  ldo)
            rdo (if (= (org rdi) (org rdo))
                  initial-cross-edge
                  rdo)]
          ;;
          ;; This is the merge loop:
          ;;
          (loop [cross-edge initial-cross-edge]
            (println "cross-edge: " (verts cross-edge))
            (let [l-candidate (bubble-left! cross-edge)
                  r-candidate (bubble-right! cross-edge)
                  _ (println)
                  _ (println "candidates: l: " (verts l-candidate)
                             " r: " (verts r-candidate))
                  ;;
                  dest-above-cross-edge? (fn [edge] (dest-above? edge cross-edge))]

              (println "l-cand above? " (dest-above-cross-edge? l-candidate))
              (println "r-cand above? " (dest-above-cross-edge? r-candidate))
              ;;
              ;; If neither (dest l-candidate) nor (dest r-candidate) is above cross-edge,
              ;; then cross-edge is the upper common tangent and we're done.
              ;;
              ;; Otherwise:
              (if (or (dest-above-cross-edge? l-candidate)
                      (dest-above-cross-edge? r-candidate))
                (do
                ;; The next cross edge is to be connected to either
                ;; (dest l-candidate) or (dest r-candidate).
                ;; If both dests are above cross-edge,
                ;; then choose the appropriate one using the in-circle? test:
                (if (or (not (dest-above-cross-edge? l-candidate))
                        (and (dest-above-cross-edge? r-candidate)
                             (in-circle? (dest l-candidate) (org l-candidate) (org r-candidate)
                                         (dest r-candidate))))
                  ;; Add new cross edge from (dest r-candidate) to (dest cross-edge):
                  (recur (connect! r-candidate (sym cross-edge)))
                  ;; Else add new cross edge from (org cross-edge) to (dest l-candidate):
                  (recur (connect! (sym cross-edge) (sym l-candidate)))))
                (println "Done at upper common tangent"))))

        [ldo rdo])))

(defn delaunay
  "Run delaunay' on sorted sites with no duplicates.
  This makes all future splittings constant-time operations."
  [sites]
  (println "entering delaunay")
  (delaunay' (sort-xy (vec (distinct sites)))))
