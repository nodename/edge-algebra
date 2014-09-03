(ns thi.ng.geom.core.quaternion
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [V3X V3Y V3Z]]
   [thi.ng.geom.core.matrix :as mat]
   [thi.ng.common.error :as err]
   [thi.ng.common.math.core :as m :refer [*eps* PI TWO_PI]]
   #+clj [thi.ng.macromath.core :as mm])
  #+cljs
  (:require-macros
   [thi.ng.macromath.core :as mm])
  #+clj
  (:import
   [thi.ng.geom.core.vector Vec3]
   [thi.ng.geom.core.matrix Matrix44]))

(declare swizzle4 swizzle4-assoc)

(deftype Quat4
  #+clj  [^double x ^double y ^double z ^double w]
  #+cljs [x y z w]
#+clj clojure.lang.ILookup
#+clj (valAt
       [_ k] (swizzle4 _ k nil))
#+clj (valAt
       [_ k nf] (swizzle4 _ k nf))

#+clj java.util.concurrent.Callable
#+clj (call
       [_] (.invoke ^clojure.lang.IFn _))
#+clj java.lang.Runnable
#+clj (run
        [_] (.invoke ^clojure.lang.IFn _))

#+clj clojure.lang.IFn
#+clj (invoke
       [_ k] (swizzle4 _ k nil))
#+clj (invoke
       [_ k nf] (swizzle4 _ k nf))
#+clj (applyTo
       [_ args]
       (condp = (count args)
         1 (swizzle4 _ (first args) nil)
         2 (swizzle4 _ (first args) (second args))
         (err/arity-error! (count args))))

#+clj clojure.lang.Associative
#+clj clojure.lang.IPersistentVector
#+clj (count
       [_] 4)
#+clj (length
       [_] 4)
#+clj (containsKey
       [_ k] (not (nil? (#{0 1 2 3 :x :y :z :w} k))))
#+clj (assoc
          [_ k v] (swizzle4-assoc _ k v))
#+clj (assocN
       [_ k v]
       (case (int k)
         0 (Quat4. v y z w)
         1 (Quat4. x v z w)
         2 (Quat4. x y v w)
         3 (Quat4. x y z v)
         (err/key-error! k)))

#+clj java.util.Collection
#+clj (isEmpty
       [_] false)
#+clj (iterator
       [_] (.iterator ^java.util.Collection (list x y z w)))
#+clj (toArray
       [_] (double-array 4 [x y z w]))
#+clj (size
       [_] 4)

#+clj clojure.lang.IPersistentCollection
#+clj clojure.lang.Indexed
#+clj clojure.lang.Sequential
#+clj clojure.lang.ISeq
#+clj clojure.lang.Seqable
#+clj (first
       [_] x)
#+clj (next
       [_] (cons y (cons z (cons w nil))))
#+clj (more
       [_] (cons y (cons z (cons w nil))))
#+clj (cons
       [_ v] [x y z w v])
#+clj (peek
       [_] w)
#+clj (pop
       [_] (thi.ng.geom.core.vector.Vec3. x y z))
#+clj (rseq
       [_] (Quat4. w z y x))
#+clj (seq
       [_] _)
#+clj (nth
       [_ k] (case (int k) 0 x, 1 y, 2 z, 3 w, (err/key-error! k)))
#+clj (nth
       [_ k nf] (case (int k) 0 x, 1 y, 2 z, 3 w, nf))
#+clj (equiv
       [_ o]
       (if (instance? Quat4 o)
         (and (clojure.lang.Numbers/equiv x (.-x ^Quat4 o))
              (clojure.lang.Numbers/equiv y (.-y ^Quat4 o))
              (clojure.lang.Numbers/equiv z (.-z ^Quat4 o))
              (clojure.lang.Numbers/equiv w (.-w ^Quat4 o)))
         (and (instance? java.util.Collection o)
              (= 4 (count o))
              (clojure.lang.Util/equiv x (nth o 0))
              (clojure.lang.Util/equiv y (nth o 1))
              (clojure.lang.Util/equiv z (nth o 2))
              (clojure.lang.Util/equiv w (nth o 3)))))
#+clj (equals
       [_ o]
       (if (instance? Quat4 o)
         (and (clojure.lang.Util/equals x (.-x ^Quat4 o))
              (clojure.lang.Util/equals y (.-y ^Quat4 o))
              (clojure.lang.Util/equals z (.-z ^Quat4 o))
              (clojure.lang.Util/equals w (.-w ^Quat4 o)))
         (and (instance? java.util.Collection o)
              (= 4 (count o))
              (clojure.lang.Util/equals x (nth o 0))
              (clojure.lang.Util/equals y (nth o 1))
              (clojure.lang.Util/equals z (nth o 2))
              (clojure.lang.Util/equals w (nth o 3)))))

#+clj Comparable
#+clj (compareTo
       [_ o]
       (if (instance? Quat4 o)
         (let [c (compare x (.-x ^Quat4 o))]
           (if (== 0 c)
             (let [c (compare y (.-y ^Quat4 o))]
               (if (== 0 c)
                 (let [c (compare z (.-z ^Quat4 o))]
                   (if (== 0 c)
                     (compare w (.-w ^Quat4 o))
                     c))
                 c))
             c))
         (let [c (count o)]
           (if (= 4 c) (compare o _) (- 4 c)))))
#+clj (hashCode
       [_]
       (-> 31
           (unchecked-add-int (hash x))
           (unchecked-multiply-int 31)
           (unchecked-add-int (hash y))
           (unchecked-multiply-int 31)
           (unchecked-add-int (hash z))
           (unchecked-multiply-int 31)
           (unchecked-add-int (hash w))))

#+clj clojure.lang.IHashEq
#+clj (hasheq
       [_]
       (mix-collection-hash
        (-> 31
            (unchecked-add-int (hash x))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash y))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash z))
            (unchecked-multiply-int 31)
            (unchecked-add-int (hash w)))
        4))

Object
(toString
 [_] (str "[" x " " y " " z " " w "]"))
#+clj (clone [_] (Quat4. x y z w))
#+cljs #_ ICloneable
#+cljs #_ (-clone
        [_] (Quat4. x y z w))

#+cljs ILookup
#+cljs (-lookup
        [_ k] (swizzle4 _ k nil))
#+cljs (-lookup
        [_ k nf] (swizzle4 _ k nf))

#+cljs IFn
#+cljs (-invoke
        [_ k] (swizzle4 _ k nil))
#+cljs (-invoke
        [_ k nf] (swizzle4 _ k nf))

#+cljs ICounted
#+cljs (-count
        [_] 4)

#+cljs IAssociative
#+cljs (-contains-key?
        [_ k] (not (nil? (#{0 1 2 3 :x :y :z :w} k))))
#+cljs (-assoc
        [_ k v] (swizzle4-assoc _ k v))

#+cljs IVector
#+cljs (-assoc-n
        [_ k v]
        (case (int k)
          0 (Quat4. v y z w)
          1 (Quat4. x v z w)
          2 (Quat4. x y v w)
          3 (Quat4. x y z v)
          (err/key-error! k)))

#+cljs ISequential
#+cljs ISeq
#+cljs (-first
        [_] x)
#+cljs (-rest
        [_] (cons y (cons z (cons w nil))))

#+cljs INext
#+cljs (-next
        [_] (cons y (cons z (cons w nil))))

#+cljs ISeqable
#+cljs (-seq
        [_] _)

#+cljs IReversible
#+cljs (-rseq
        [_] (Quat4. w z y x))

#+cljs IIndexed
#+cljs (-nth
        [_ k] (case (int k) 0 x, 1 y, 2 z, 3 w, (err/key-error! k)))
#+cljs (-nth
        [_ k nf] (case (int k) 0 x, 1 y, 2 z, 3 w, nf))

#+cljs ICollection
#+cljs (-conj
        [_ v] [x y z w v])

#+cljs IStack
#+cljs (-peek
        [_] w)
#+cljs (-pop
        [_] (thi.ng.geom.core.vector.Vec3. x y z))

#+cljs IComparable
#+cljs (-compare
        [_ o]
        (if (instance? Quat4 o)
          (let [c (compare x (.-x ^Quat4 o))]
            (if (== 0 c)
              (let [c (compare y (.-y ^Quat4 o))]
                (if (== 0 c)
                  (let [c (compare z (.-z ^Quat4 o))]
                    (if (== 0 c)
                      (compare w (.-w ^Quat4 o))
                      c))
                  c))
              c))
          (let [c (count o)]
            (if (= 4 c) (compare o _) (- 4 c)))))

#+cljs IHash
#+cljs (-hash
        [_] (-> (hash x)
                (hash-combine (hash y))
                (hash-combine (hash z))))

#+cljs IEquiv
#+cljs (-equiv
        [_ o]
        (if (instance? Quat4 o)
          (and (== x (.-x ^Quat4 o)) (== y (.-y ^Quat4 o))
               (== z (.-z ^Quat4 o)) (== w (.-w ^Quat4 o)))
          (and (sequential? o) (= 4 (count o))
               (= x (nth o 0)) (= y (nth o 1))
               (= z (nth o 2)) (= w (nth o 3)))))
g/PScale
(scale
 [_ s]
 (Quat4. (* x s) (* y s) (* z s) (* w s)))
g/PRotate3D
(rotate-x
 [_ theta]
 (let [t (/ theta 2.0)
       s (Math/sin t) c (Math/cos t)]
   (Quat4.
    (mm/madd x c w s)
    (mm/madd y c z s)
    (mm/msub z c y s)
    (mm/msub w c x s))))
(rotate-y
 [_ theta]
 (let [t (/ theta 2.0)
       s (Math/sin t) c (Math/cos t)]
   (Quat4.
    (mm/msub x c z s)
    (mm/madd y c w s)
    (mm/madd z c x s)
    (mm/msub w c y s))))
(rotate-z
 [_ theta]
 (let [t (/ theta 2.0)
       s (Math/sin t) c (Math/cos t)]
   (Quat4.
    (mm/madd x c y s)
    (mm/msub y c x s)
    (mm/madd z c w s)
    (mm/msub w c z s))))
g/PMathOps
(+
 [_ q]
 (let [q ^Quat4 q]
   (Quat4.
    (+ x (.-x q))
    (+ y (.-y q))
    (+ z (.-z q))
    (+ w (.-w q)))))
(-
 [_ q]
 (let [q ^Quat4 q]
   (Quat4.
    (- x (.-x q))
    (- y (.-y q))
    (- z (.-z q))
    (- w (.-w q)))))
(*
 [_ q]
 (let [q ^Quat4 q
       qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)]
   (Quat4.
    (mm/maddsub x qw w qx y qz z qy)
    (mm/maddsub y qw w qy z qx x qz)
    (mm/maddsub z qw w qz x qy y qx)
    (mm/msub w qw x qx y qy z qz))))
(*
 [_ q r]
 (let [q ^Quat4 q r ^Quat4 r
       qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)
       rx (.-x r) ry (.-y r) rz (.-z r) rw (.-w r)
       x' (mm/maddsub x qw w qx y qz z qy)
       y' (mm/maddsub y qw w qy z qx x qz)
       z' (mm/maddsub z qw w qz x qy y qx)
       w' (mm/msub w qw x qx y qy z qz)]
   (Quat4.
    (mm/maddsub x' rw w' rx y' rz z' ry)
    (mm/maddsub y' rw w' ry z' rx x' rz)
    (mm/maddsub z' rw w' rz x' ry y' rx)
    (mm/msub w' rw x' rx y' ry z' rz))))
g/PDotProduct
(dot
 [_ q]
 (let [q ^Quat4 q]
   (mm/madd x (.-x q) y (.-y q) z (.-z q) w (.-w q))))
g/PMagnitude
(mag
 [_] (Math/sqrt (mm/madd x x y y z z w w)))
(mag-squared
 [_] (mm/madd x x y y z z w w))
g/PNormalize
(normalize [_]
  (let [m (Math/sqrt (mm/madd x x y y z z w w))]
    (if (> m *eps*)
      (Quat4. (/ x m) (/ y m) (/ z m) (/ w m))
      _)))
g/PConjugate
(conjugate [_]
  (Quat4. (- x) (- y) (- z) w))
g/PInvert
(invert [_]
  (let [d (g/mag-squared _)
        d (if (zero? d) 0.0 (/ d))
        id (- d)]
    (Quat4. (* x id) (* y id) (* z id) (* w d))))
g/PInterpolate
(mix [_ q] (g/mix _ q 0.5))
(mix [_ q t]
  (let [d (g/dot _ q)]
    (if (< (m/abs d) 1.0)
      (let [theta (Math/acos d)
            stheta (Math/sqrt (mm/madd d d -1.0))
            [a b] (if (< (m/abs stheta) 0.001)
                    [0.5 0.5]
                    [(/ (Math/sin (mm/subm 1.0 t theta)) stheta)
                     (/ (Math/sin (* t theta)) stheta)])
            q ^Quat4 q]
        (Quat4.
         (mm/madd x a (.-x q) b)
         (mm/madd y a (.-y q) b)
         (mm/madd z a (.-z q) b)
         (mm/madd w a (.-w q) b)))
      _)))
g/PMatrixConvert
(as-matrix
 [_]
 (let [x2 (+ x x)
       y2 (+ y y)
       z2 (+ z z)
       xx (* x x2)
       yx (* y x2)
       yy (* y y2)
       zx (* z x2)
       zy (* z y2)
       zz (* z z2)
       wx (* w x2)
       wy (* w y2)
       wz (* w z2)]
   (thi.ng.geom.core.matrix.Matrix44.
    (mm/sub 1.0 yy zz) ;m00
    (- yx wz)     ;m10
    (+ zx wy)     ;m20
    0.0
    (+ yx wz)     ;m01
    (mm/sub 1.0 xx zz) ;m11
    (- zy wx)     ;m21
    0.0
    (- zx wy)     ;m02
    (+ zy wx)     ;m12
    (mm/sub 1.0 xx yy) ;m22
    0.0
    0.0 0.0 0.0 1.0)))
g/PVectorTransform
(transform-vector
 [_ [vx vy vz :as v]]
 (let [ix (mm/maddsub w vx y vz z vy)
       iy (mm/maddsub w vy z vx x vz)
       iz (mm/maddsub w vz x vy y vx)
       nx (- x) ny (- y) nz (- z)
       iw (mm/msub nx vx y vy z vz)]
   (thi.ng.geom.core.vector.Vec3.
    (mm/maddsub ix w iw nx iy nz iz ny)
    (mm/maddsub iy w iw ny iz nx ix nz)
    (mm/maddsub iz w iw nz ix ny iy nx))))
)
#+clj (defmethod print-method Quat4 [o ^java.io.Writer w] (.write w (.toString o)))

(defn quat
  ([] (Quat4. 0.0 0.0 0.0 1.0))
  ([[x y z] w] (Quat4. x y z w))
  ([[x y z w]] (Quat4. x y z w))
  ([x y z w] (Quat4. x y z w)))

(defn quat-from-axis-angle
  [axis theta]
  (let [theta (/ theta 2.0)]
    (quat (g/normalize (v/vec3 axis) (Math/sin theta)) (Math/cos theta))))

(defn quat-from-euler
  [order & [alpha beta gamma]]
  (let [[a b c] (case order
                  :xyz [V3X V3Y V3Z]
                  :yxz [V3Y V3X V3Z]
                  :xzy [V3X V3Z V3Y]
                  :zxy [V3Z V3X V3Y]
                  :yzx [V3Y V3Z V3X]
                  :zyx [V3Z V3Y V3X]
                  (err/illegal-arg! order))]
    (g/* (quat-from-axis-angle a alpha)
           (quat-from-axis-angle b beta)
           (quat-from-axis-angle c gamma))))

;; Allan and Mark Watt's "Advanced Animation and Rendering Techniques"
;; (ACM Press 1992)
(defn quat-from-matrix
  [^Matrix44 m]
  (let [trace (mm/add (.-m00 m) (.-m11 m) (.-m22 m))]
    (if (pos? trace)
      (let [s (Math/sqrt (inc trace))
            w (/ s 2.0)
            s (/ 0.5 s)]
        (Quat4.
         (mm/subm (.-m21 m) (.-m12 m) s)
         (mm/subm (.-m02 m) (.-m20 m) s)
         (mm/subm (.-m10 m) (.-m01 m) s) w))
      (let [[i ii] (if (> (.-m11 m) (.-m00 m))
                     [1 (.-m11 m)] [0 (.-m00 m)])
            [i ii] (if (> (.-m22 m) ii)
                     [2 (.-m22 m)] [i ii])
            [jj kk jk kj ij ji ik ki]
            (case i
              0 [(.-m11 m) (.-m22 m) (.-m12 m) (.-m21 m) (.-m01 m) (.-m10 m) (.-m02 m) (.-m20 m)]
              1 [(.-m22 m) (.-m00 m) (.-m20 m) (.-m02 m) (.-m12 m) (.-m21 m) (.-m10 m) (.-m01 m)]
              2 [(.-m00 m) (.-m11 m) (.-m01 m) (.-m10 m) (.-m20 m) (.-m02 m) (.-m21 m) (.-m12 m)])
            s (Math/sqrt (inc (- ii (+ jj kk))))
            x (/ s 2.0)
            s (/ 0.5 s)
            qj (mm/addm ij ji s)
            qk (mm/addm ik ki s)
            qw (mm/subm kj jk s)]
        (case i
          0 (Quat4. x qj qk qw)
          1 (Quat4. qk x qj qw)
          2 (Quat4. qj qk x qw))))))

(defn alignment-quat
  [a b]
  (let [d (g/dot a b)]
    (cond
     (< d -0.999999) (let [c (g/cross V3X a)
                           c (if (< (g/mag c) 1e-6) (g/cross V3Y a) c)]
                       (quat-from-axis-angle c PI))
     (> d 0.999999) (quat)
     :default (g/normalize (quat (g/cross a b) (inc d))))))

(defn lookup4
  [^Quat4 _ k nf]
  (case k
    \x (.-x _)
    \y (.-y _)
    \z (.-z _)
    \w (.-w _)
    (or nf (err/key-error! k))))

(defn swizzle4
  [^Quat4 _ k default]
  (if (number? k)
    (case (int k)
      0 (.-x _)
      1 (.-y _)
      2 (.-z _)
      3 (.-w _)
      (or default (err/key-error! k)))
    (case k
      :x (.-x _)
      :y (.-y _)
      :z (.-z _)
      :w (.-w _)
      (let [n (name k) c (count n)]
        (case c
          2 (thi.ng.geom.core.vector.Vec2.
             (lookup4 _ (nth n 0) default)
             (lookup4 _ (nth n 1) default))
          3 (thi.ng.geom.core.vector.Vec3.
             (lookup4 _ (nth n 0) default)
             (lookup4 _ (nth n 1) default)
             (lookup4 _ (nth n 2) default))
          4 (Quat4.
             (lookup4 _ (nth n 0) default)
             (lookup4 _ (nth n 1) default)
             (lookup4 _ (nth n 2) default)
             (lookup4 _ (nth n 3) default))
          (or default (err/key-error! k)))))))

(defn swizzle4-assoc
  [^Quat4 _ k v]
  (if (number? k)
    (case (int k)
      0 (Quat4. v (.-y _) (.-z _) (.-w _))
      1 (Quat4. (.-x _) v (.-z _) (.-w _))
      2 (Quat4. (.-x _) (.-y _) v (.-w _))
      3 (Quat4. (.-x _) (.-y _) (.-z _) v)
      (err/key-error! k))
    (case k
      :x (Quat4. v (.-y _) (.-z _) (.-w _))
      :y (Quat4. (.-x _) v (.-z _) (.-w _))
      :z (Quat4. (.-x _) (.-y _) v (.-w _))
      :w (Quat4. (.-x _) (.-y _) (.-z _) v)
      (v/swizzle-assoc* _ quat {\x 0 \y 1 \z 2 \w 3} k v))))
