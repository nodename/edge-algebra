(ns edge-algebra.delaunay
  (:require
   ;; the two topological operations exported by the edge-algebra library:
   [edge-algebra.core :refer [make-edge! splice!]]
   ;; some neighboring-edge properties:
   [edge-algebra.edge :refer [sym l-next o-next o-prev r-prev]]
   ;;
   ;; geometry support from toxi's geom library:
   [thi.ng.geom.core.vector :refer [vec2]]
   [thi.ng.geom.core.matrix :refer [matrix44]]
   [thi.ng.geom.core.utils :refer [norm-sign2]])

  (:refer-clojure :exclude [swap!]))

(def pt vec2)


;; For the Delaunay application, the data field of an Edge will contain the coordinates of its origin.
(defn org
  [edge]
  (.getData edge))

(defn set-org!
  [edge coords]
  (.setData edge coords))

;; and similarly, the data field of its symmetric Edge will contain the coordinates of its destination.
(defn dest
  [edge]
  (org (sym edge)))

(defn set-dest!
  [edge coords]
  (.setData (sym edge) coords))




(defn connect!
  [a b]
  (let [e (make-edge!)]
    (set-org! e (dest a))
    (set-dest! e (org b))
    (splice! e (l-next a))
    (splice! (sym e) b)
    e))

(defn delete-edge!
  [e]
  (splice! e (o-prev e))
  (splice! (sym e) (o-prev (sym e))))


;; for the incremental algorithm:
(defn swap!
  [e]
  (let [a (o-prev e)
        b (o-prev (sym e))]
    (splice! e a)
    (splice! (sym e) b)
    (splice! e (l-next a))
    (splice! (sym e) (l-next b))
    (set-org! e (dest a))
    (set-dest! e (dest b))))




(defn in-circle?
  "The predicate in-circle(a, b, c, d) is defined to be true if and only if
  point d is interior to the region of the plane that is bounded by the
  oriented circle abc and lies to the left of it."
  [a b c d]
  (let [m (matrix44 (.-x a) (.-y a) (.mag-squared a) 1
                    (.-x b) (.-y b) (.mag-squared b) 1
                    (.-x c) (.-y c) (.mag-squared c) 1
                    (.-x d) (.-y d) (.mag-squared d) 1)]

    (> (.determinant m) 0)))


(defn ccw?
  "The predicate ccw is true if the points a, b, and c form a counterclockwise-oriented triangle."
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
    ;(println "deleting edge" edge)
    (delete-edge! edge)
    t))

(defn slide-right!
  [edge]
  (let [t (o-prev edge)]
    ;(println "deleting edge" edge)
    (delete-edge! edge)
    t))


(defn valid?
  [edge basel]
  {:pre [(some? edge) (some? basel)]}
  (right-of? (dest edge) basel))

(defn bubble-left!
  [basel]
  (let [lcand (o-next (sym basel))]
    (if (valid? lcand basel)
      (loop [x lcand]
        (if (in-circle? (dest basel) (org basel) (dest x) (dest (o-next x)))
          (recur (slide-left! x))
          x))
      lcand)))

(defn bubble-right!
  [basel]
  (let [rcand (o-prev basel)]
    (if (valid? rcand basel)
      (loop [x rcand]
        (if (in-circle? (dest basel) (org basel) (dest x) (dest (o-prev x)))
          (recur (slide-right! x))
          x))
      rcand)))

(defn delaunay
  "Calculate the Delaunay triangulation of the sites and return
   the counterclockwise convex hull edge out of the leftmost vertes
   and the clockwise convex hull edge out of the rightmost vertex."
  [sites]
  (condp = (count sites)
    1 nil

    2 (let [a (make-edge!)]
        (set-org! a (sites 0))
        (set-dest! a (sites 1))
        [a (sym a)])

    3 (let [[s1 s2 s3] (sort-xy sites)
            ;; create edge a connecting s1 to s2 and edge b connecting s2 to s3:
            a (make-edge!)
            b (make-edge!)]
        (set-org! a s1)
        (set-dest! a s2)
        (set-org! b s2)
        (set-dest! b s3)
        (splice! (sym a) b)
        ;; now close the triangle:
        (cond
         (ccw? s1 s2 s3) (do
                           (connect! b a)
                           [a (sym b)])
         (ccw? s1 s3 s2) (do
                           (let [c (connect! b a)]
                             [(sym c) c]))
         ;; otherwise the three points are collinear:
         :else [a (sym b)]))
    ;;
    ;; The default case, four or more sites: divide and conquer
    ;;
    (let [[l r] (halves sites)
          [ldo ldi] (delaunay l)
          [rdi rdo] (delaunay r)
          ;; compute the lower common tangent [ldi rdi] of l and r:
          [ldi rdi] (cond
                     (left-of? (org rdi) ldi)  [(l-next ldi) rdi]
                     (right-of? (org ldi) rdi) [ldi (r-prev rdi)]
                     :else                     [ldi rdi])
          ;;
          ;; create a first cross edge initial-basel from (org rdi) to (org ldi):
          initial-basel (connect! (sym rdi) ldi)
          ldo (if (= (org ldi) (org ldo))
                (sym initial-basel)
                ldo)
          rdo (if (= (org rdi) (org rdo))
                initial-basel
                rdo)]
        ;;
        ;; this is the merge loop:
        ;;
        (loop [basel initial-basel]
          ;; locate the first l point (dest lcand) to be encountered by the rising bubble,
          ;; and delete l edges out of (dest basel) that fail the circle test:
          (let [lcand (bubble-left! basel)
                ;; symmetrically, locate the first r point to be hit, and delete r edges:
                rcand (bubble-right! basel)]

            ;; If lcand and rcand are both invalid,
            ;; then basel is the upper common tangent and we're done.

            ;; Otherwise:
            (when (or (valid? lcand basel) (valid? rcand basel))
              ;; the next cross edge is to be connected to either (dest lcand) or (dest rcand).
              ;; if both are valid, then choose the appropriate one using the in-circle? test:
              (if (or (not (valid? lcand basel))
                      (and (valid? rcand basel)
                           (in-circle? (dest lcand) (org lcand) (org rcand) (dest rcand))))
                ;; add cross edge from (dest rcand) to (dest basel):
                (recur (connect! rcand (sym basel)))
                ;; else add cross edge from (org basel) to (dest lcand):
                (recur (connect! (sym basel) (sym lcand)))))))

      [ldo rdo])))



