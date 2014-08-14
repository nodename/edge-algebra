(ns delaunay.div-conq
  (:require
   ;; the two topological operations exported by the edge-algebra library
   ;; are make-edge!, which we use to define our own make-edge! (see below),
   ;; and splice!:
   [edge-algebra.core :as core :refer [splice!]]
   ;;
   ;; some functions for navigating to related edges:
   [edge-algebra.edge :as e :refer [sym o-next o-prev l-next r-prev]]
   ;;
   ;; geometry support from toxi's geom library:
   [thi.ng.geom.core.vector :refer [vec2]]
   [thi.ng.geom.core.matrix :refer [matrix44]]
   [thi.ng.geom.core.utils :refer [norm-sign2]]
   ;;
   [delaunay.utils.reporting :refer [wrap-with-name-and-args-reporting]]))

;; An alias for the 2-D point constructor:
(def pt vec2)


;; For the Delaunay application, the data field of an Edge
;; will contain the coordinates of its origin:
(defn org
  [edge]
  (#+clj .getData #+cljs e/getData edge))

(defn set-org!
  [edge coords]
  (#+clj .setData #+cljs e/setData edge coords))

;; and similarly, the data field of its symmetric Edge
;; will contain the coordinates of its destination:
(defn dest
  [edge]
  (org (sym edge)))

(defn set-dest!
  [edge coords]
  (.setData (sym edge) coords)
  ;; previous line returns (sym edge) ha ha, so:
  edge)



(defn make-edge!
  [org dest]
  (-> (core/make-edge!)
    (set-org! org)
    (set-dest! dest)))


(defn connect!
  [a b]
  (let [e (make-edge! (dest a) (org b))]
    (splice! e (l-next a))
    (splice! (sym e) b)
    e))

(defn delete-edge!
  [e]
  (splice! e (o-prev e))
  (splice! (sym e) (o-prev (sym e))))


;; In addition to the two topological operations,
;; we need just two geometric primitives: in-circle? and ccw?

(defn in-circle?
  "The predicate in-circle? is defined to be true if and only if
  point d is interior to the region of the plane that is bounded by the
  oriented circle abc and lies to the left of it."
  [a b c d]
  (let [m (matrix44 (.-x a) (.-y a) (.mag-squared a) 1
                    (.-x b) (.-y b) (.mag-squared b) 1
                    (.-x c) (.-y c) (.mag-squared c) 1
                    (.-x d) (.-y d) (.mag-squared d) 1)]

    (> (.determinant m) 0)))


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
  (let [half-way (/ (count seq) 2)
        sorted-seq (sort-xy seq)]
    (map vec (split-at half-way sorted-seq))))


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

(defn delaunay
  "Calculate the Delaunay triangulation of the sites; return
   the counterclockwise convex hull edge out of the leftmost vertex
   and the clockwise convex hull edge out of the rightmost vertex."
  [sites]
  (let [sites (vec (distinct sites))]
    (condp = (count sites)
      1 nil

      2 (let [a (make-edge! (sites 0) (sites 1))]
          [a (sym a)])

      3 (let [[s1 s2 s3] (sort-xy sites)
              a (make-edge! s1 s2)
              b (make-edge! s2 s3)]
          (splice! (sym a) b)
          ;; Now close the triangle:
          (cond
           (ccw? s1 s2 s3) (do
                             (connect! b a)
                             [a (sym b)])
           (ccw? s1 s3 s2) (do
                             (let [c (connect! b a)]
                               [(sym c) c]))
           ;; Otherwise the three points are collinear:
           :else [a (sym b)]))
      ;;
      ;; The default case, four or more sites: divide and conquer
      ;;
      (let [[l r] (halves sites)
            [ldo ldi] (delaunay l)
            [rdi rdo] (delaunay r)
            ;; Compute the lower common tangent [ldi rdi] of l and r:
            [ldi rdi] (cond
                       (left-of? (org rdi) ldi)  [(l-next ldi) rdi]
                       (right-of? (org ldi) rdi) [ldi (r-prev rdi)]
                       :else                     [ldi rdi])
            ;;
            ;; Create initial-cross-edge from (org rdi) to (org ldi)
            ;; (Note that we always choose the right-to-left direction for a cross-edge):
            initial-cross-edge (connect! (sym rdi) ldi)
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
            (let [l-candidate (bubble-left! cross-edge)
                  r-candidate (bubble-right! cross-edge)
                  ;;
                  dest-above-cross-edge? (fn [edge] (dest-above? edge cross-edge))]
              ;;
              ;; If neither (dest l-candidate) nor (dest r-candidate) is above cross-edge,
              ;; then cross-edge is the upper common tangent and we're done.
              ;;
              ;; Otherwise:
              (when (or (dest-above-cross-edge? l-candidate) (dest-above-cross-edge? r-candidate))
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
                  (recur (connect! (sym cross-edge) (sym l-candidate)))))))

        [ldo rdo]))))


(defn with-reporting
  [ch f & [args]]
  (with-redefs [make-edge! (wrap-with-name-and-args-reporting ch make-edge!)
                delete-edge! (wrap-with-name-and-args-reporting ch delete-edge!)
                in-circle? (wrap-with-name-and-args-reporting ch in-circle?)]
    (f args)))
