(ns thi.ng.geom.core.matrix
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v]
   [thi.ng.common.math.core :as m :refer [*eps* PI TWO_PI]]
   [thi.ng.common.error :as err]
   #+clj [thi.ng.macromath.core :as mm]
   #+clj [thi.ng.geom.core.macros :as gmc])
  #+cljs
  (:require-macros
   [thi.ng.macromath.core :as mm]
   [thi.ng.geom.core.macros :as gmc])
  #+clj
  (:import
   [thi.ng.geom.core.vector Vec2 Vec3]))

#+clj
(defn- hash-coll*
  [coll]
  (reduce
   #(-> % (unchecked-multiply-int 31) (unchecked-add-int (hash %2)))
   1 coll))

#+clj
(defn- hash-matrix
  [coll]
  (mix-collection-hash (hash-coll* coll) (count coll)))

#+cljs
(defn- hash-matrix
  [coll]
  (loop [res (hash (first coll)) s (next coll)]
    (if (nil? s)
      res
      (recur (hash-combine res (hash (first s))) (next s)))))

(deftype Matrix32
  #+clj  [^double m00 ^double m01 ^double m02
          ^double m10 ^double m11 ^double m12]
  #+cljs [m00 m01 m02 m10 m11 m12]
#+clj java.util.Collection
#+clj clojure.lang.IPersistentCollection
#+clj clojure.lang.IPersistentVector
#+clj clojure.lang.ISeq
#+clj clojure.lang.Seqable
#+clj clojure.lang.Sequential
#+clj clojure.lang.IHashEq
#+cljs ISequential
#+clj (empty [_] (err/unsupported!))

#+cljs ICounted
(#+clj count #+cljs -count [_] 6)

#+cljs ISeqable
(#+clj seq   #+cljs -seq   [_] _)

#+cljs ISeq
(#+clj more  #+cljs -rest  [_] (next _))
(#+clj first #+cljs -first [_] m00)
#+cljs INext
(#+clj next  #+cljs -next  [_] (seq [m01 m02 m10 m11 m12]))

#+cljs ICollection
(#+clj cons  #+cljs -conj  [_ x] [m00 m01 m02 m10 m11 m12 x])

#+cljs IIndexed
(#+clj nth   #+cljs -nth [_ k]
       (if (m/in-range? 0 5 k)
         (case (int k)
           0 m00 1 m01 2 m02 3 m10 4 m11 5 m12)
         (err/illegal-arg! k)))
(#+clj nth   #+cljs -nth [_ k nf]
       (if (m/in-range? 0 15 k)
         (case (int k)
           0 m00 1 m01 2 m02 3 m10 4 m11 5 m12)
         nf))

#+cljs IEquiv
(#+clj equiv  #+cljs -equiv [_ o]
       (if (and (sequential? o) (= 6 (count o)))
         (every? #(= (% 0) (% 1)) (map vector _ o))
         false))

#+cljs IHash
(#+clj hasheq #+cljs -hash  [_] (hash-matrix _))

#+clj (hashCode [_] (hash-coll* _))
#+clj (equals
       [_ o]
       (if (and (sequential? o) (= 6 (count o)))
         (every? #(clojure.lang.Util/equals (% 0) (% 1)) (map vector _ o))
         false))
#+clj (isEmpty
       [_] false)
#+clj (iterator
       [_] (.iterator ^java.util.Collection
                      (list m00 m01 m02 m10 m11 m12)))
#+clj (toArray
       [_] (object-array _))
#+clj (size [_] 6)
#+clj (length [_] 6)

Object
(toString
 [_] (apply str (concat "[" (interpose \space _) "]")))
g/PMathOps
(*
 [_ m]
 (let [m ^Matrix32 m]
   (Matrix32.
    (mm/madd m00 (.-m00 m) m01 (.-m10 m))
    (mm/madd m00 (.-m01 m) m01 (.-m11 m))
    (mm/madd m00 (.-m02 m) m01 (.-m12 m) m02)
    (mm/madd m10 (.-m00 m) m11 (.-m10 m))
    (mm/madd m10 (.-m01 m) m11 (.-m11 m))
    (mm/madd m10 (.-m02 m) m11 (.-m12 m) m12))))
g/PDeterminant
(determinant
 [_] (mm/msub m00 m11 m01 m10))
g/PInvert
(invert
 [_]
 (let [d (g/determinant _)]
   (when-not (zero? d)
     (Matrix32.
      (/ m11 d) (- (/ m01 d)) (/ (mm/msub m01 m12 m11 m02) d)
      (- (/ m10 d)) (/ m00 d) (/ (mm/msub m10 m02 m00 m12) d)))))
g/PRotate
(rotate
 [_ theta]
 (let [s (Math/sin theta), c (Math/cos theta)]
   (g/* _ (Matrix32. c (- s) 0.0, s c 0.0))))

g/PScale
(scale
 [_ s]
 (g/* _ (Matrix32.
           (if (number? s) s (s 0)) 0.0 0.0
           0.0 (if (number? s) s (s 1)) 0.0)))
(scale
 [_ sx sy]
 (if (number? sx)
   (if (number? sy)
     (g/* _ (Matrix32. sx 0.0 0.0, 0.0 sy 0.0))
     (g/* _ (Matrix32. (* sx (sy 0)) 0.0 0.0, 0.0 (* sx (sy 1)) 0.0)))
   (if (number? sy)
     (g/* _ (Matrix32. (* sy (sx 0)) 0.0 0.0, 0.0 (* sy (sx 1)) 0.0))
     (g/* _ (Matrix32. (* (sx 0) (sy 0)) 0.0 0.0, 0.0 (* (sx 1) (sy 1)) 0.0)))))

g/PShear
(shear
 [_ s]
 (g/* _ (Matrix32.
           1.0 (if (number? s) s (s 0)) 0.0,
           (if (number? s) s (s 1)) 1.0 0.0)))
(shear
 [_ sx sy]
 (g/* _ (Matrix32. 1.0 sx 0.0, sy 1.0 0.0)))

g/PTranslate
(translate
 [_ t]
 (g/* _ (Matrix32.
           1.0 0.0 (if (number? t) t (t 0))
           0.0 1.0 (if (number? t) t (t 1)))))
(translate
 [_ tx ty]
 (if (number? tx)
   (if (number? ty)
     (g/* _ (Matrix32. 1.0 0.0 tx, 0.0 1.0 ty))
     (g/* _ (Matrix32. 1.0 0.0 (* tx (ty 0)), 0.0 1.0 (* tx (ty 1)))))
   (if (number? ty)
     (g/* _ (Matrix32. 1.0 0.0 (* ty (tx 0)), 0.0 1.0 (* ty (tx 1))))
     (g/* _ (Matrix32. 1.0 0.0 (* (tx 0) (ty 0)), 0.0 1.0 (* (tx 1) (ty 1)))))))

g/PTransform
(transform
 [_ matrix] (g/* _ matrix))
g/PVectorTransform
(transform-vector
 [_ [x y :as v]]
 (thi.ng.geom.core.vector.Vec2.
  (mm/madd x m00 y m01 m02)
  (mm/madd x m10 y m11 m12)))
)

(deftype Matrix44
  #+clj  [^double m00 ^double m01 ^double m02 ^double m03
          ^double m10 ^double m11 ^double m12 ^double m13
          ^double m20 ^double m21 ^double m22 ^double m23
          ^double m30 ^double m31 ^double m32 ^double m33]
  #+cljs [m00 m01 m02 m03
          m10 m11 m12 m13
          m20 m21 m22 m23
          m30 m31 m32 m33]
#+clj java.util.Collection
#+clj clojure.lang.IPersistentCollection
#+clj clojure.lang.IPersistentVector
#+clj clojure.lang.ISeq
#+clj clojure.lang.Seqable
#+clj clojure.lang.Sequential
#+clj clojure.lang.IHashEq
#+cljs ISequential
#+clj (empty [_] (err/unsupported!))

#+cljs ICounted
(#+clj count #+cljs -count [_] 16)

#+cljs ISeqable
(#+clj seq   #+cljs -seq   [_] _)

#+cljs ISeq
(#+clj more  #+cljs -rest  [_] (next _))
(#+clj first #+cljs -first [_] m00)
#+cljs INext
(#+clj next  #+cljs -next
       [_] (seq [m01 m02 m03
                 m10 m11 m12 m13
                 m20 m21 m22 m23
                 m30 m31 m32 m33]))

#+cljs ICollection
(#+clj cons  #+cljs -conj
       [_ x]
       [m00 m01 m02 m03
        m10 m11 m12 m13
        m20 m21 m22 m23
        m30 m31 m32 m33 x])

#+cljs IIndexed
(#+clj nth   #+cljs -nth [_ k]
       (if (m/in-range? 0 15 k)
         (case (int k)
           0 m00  1 m01  2 m02  3 m03
           4 m10  5 m11  6 m12  7 m13
           8 m20  9 m21  10 m22 11 m23
           12 m30 13 m31 14 m32 15 m33)
         (err/illegal-arg! k)))
(#+clj nth   #+cljs -nth [_ k nf]
       (if (m/in-range? 0 15 k)
         (case (int k)
           0 m00  1 m01  2 m02  3 m03
           4 m10  5 m11  6 m12  7 m13
           8 m20  9 m21  10 m22 11 m23
           12 m30 13 m31 14 m32 15 m33)
         nf))

#+cljs IEquiv
(#+clj equiv  #+cljs -equiv [_ o]
       (if (and (sequential? o) (= 16 (count o)))
         (every? #(= (% 0) (% 1)) (map vector _ o))
         false))

#+cljs IHash
(#+clj hasheq #+cljs -hash  [_] (hash-matrix _))

#+clj (hashCode [_] (hash-coll* _))
#+clj (equals
       [_ o]
       (if (and (sequential? o) (= 16 (count o)))
         (every? #(clojure.lang.Util/equals (% 0) (% 1)) (map vector _ o))
         false))

#+clj (isEmpty
       [_] false)
#+clj (iterator
       [_] (.iterator ^java.util.Collection
                      (list m00 m01 m02 m03
                            m10 m11 m12 m13
                            m20 m21 m22 m23
                            m30 m31 m32 m33)))
#+clj (toArray
       [_] (object-array _))
#+clj (size [_] 16)
#+clj (length [_] 16)

Object
(toString
 [_]
 (apply str (concat "[" (interpose \space _) "]")))
g/PMathOps
(*
 [_ m]
 (let [^Matrix44 m m]
   (Matrix44.
    (mm/madd m00 (.-m00 m) m01 (.-m10 m) m02 (.-m20 m) m03 (.-m30 m))
    (mm/madd m00 (.-m01 m) m01 (.-m11 m) m02 (.-m21 m) m03 (.-m31 m))
    (mm/madd m00 (.-m02 m) m01 (.-m12 m) m02 (.-m22 m) m03 (.-m32 m))
    (mm/madd m00 (.-m03 m) m01 (.-m13 m) m02 (.-m23 m) m03 (.-m33 m))

    (mm/madd m10 (.-m00 m) m11 (.-m10 m) m12 (.-m20 m) m13 (.-m30 m))
    (mm/madd m10 (.-m01 m) m11 (.-m11 m) m12 (.-m21 m) m13 (.-m31 m))
    (mm/madd m10 (.-m02 m) m11 (.-m12 m) m12 (.-m22 m) m13 (.-m32 m))
    (mm/madd m10 (.-m03 m) m11 (.-m13 m) m12 (.-m23 m) m13 (.-m33 m))

    (mm/madd m20 (.-m00 m) m21 (.-m10 m) m22 (.-m20 m) m23 (.-m30 m))
    (mm/madd m20 (.-m01 m) m21 (.-m11 m) m22 (.-m21 m) m23 (.-m31 m))
    (mm/madd m20 (.-m02 m) m21 (.-m12 m) m22 (.-m22 m) m23 (.-m32 m))
    (mm/madd m20 (.-m03 m) m21 (.-m13 m) m22 (.-m23 m) m23 (.-m33 m))

    (mm/madd m30 (.-m00 m) m31 (.-m10 m) m32 (.-m20 m) m33 (.-m30 m))
    (mm/madd m30 (.-m01 m) m31 (.-m11 m) m32 (.-m21 m) m33 (.-m31 m))
    (mm/madd m30 (.-m02 m) m31 (.-m12 m) m32 (.-m22 m) m33 (.-m32 m))
    (mm/madd m30 (.-m03 m) m31 (.-m13 m) m32 (.-m23 m) m33 (.-m33 m)))))
g/PDeterminant
(determinant
 [_]
 (reduce
  +
  [(gmc/det-item m30 m21 m12 m03 m20 m31 m12 m03 m30 m11 m22 m03 m10 m31 m22 m03)
   (gmc/det-item m20 m11 m32 m03 m10 m21 m32 m03 m30 m21 m02 m13 m20 m31 m02 m13)
   (gmc/det-item m30 m01 m22 m13 m00 m31 m22 m13 m20 m01 m32 m13 m00 m21 m32 m13)
   (gmc/det-item m30 m11 m02 m23 m10 m31 m02 m23 m30 m01 m12 m23 m00 m31 m12 m23)
   (gmc/det-item m10 m01 m32 m23 m00 m11 m32 m23 m20 m11 m02 m33 m10 m21 m02 m33)
   (gmc/det-item m20 m01 m12 m33 m00 m21 m12 m33 m10 m01 m22 m33 m00 m11 m22 m33)]))
g/PInvert
(invert
 [_]
 (let [n00 (mm/msub m00 m11 m01 m10)
       n01 (mm/msub m00 m12 m02 m10)
       n02 (mm/msub m00 m13 m03 m10)
       n03 (mm/msub m01 m12 m02 m11)
       n04 (mm/msub m01 m13 m03 m11)
       n05 (mm/msub m02 m13 m03 m12)
       n06 (mm/msub m20 m31 m21 m30)
       n07 (mm/msub m20 m32 m22 m30)
       n08 (mm/msub m20 m33 m23 m30)
       n09 (mm/msub m21 m32 m22 m31)
       n10 (mm/msub m21 m33 m23 m31)
       n11 (mm/msub m22 m33 m23 m32)
       d (mm/madd
          n05 n06
          (mm/sub
           (mm/madd
            n03 n08
            (mm/madd
             n02 n09
             (mm/msub n00 n11 n01 n10)))
           (mm/mul n04 n07)))]
   (when-not (zero? d)
     (let [invd (/ 1.0 d)]
       (Matrix44.
        (gmc/inv-item m11 n11 m12 n10 m13 n09 invd)
        (gmc/inv-item m02 n10 m03 n09 (- m01) n11 invd)
        (gmc/inv-item m31 n05 m32 n04 m33 n03 invd)
        (gmc/inv-item m22 n04 m23 n03 (- m21) n05 invd)
        (gmc/inv-item m12 n08 m13 n07 (- m10) n11 invd)
        (gmc/inv-item m00 n11 m02 n08 m03 n07 invd)
        (gmc/inv-item m32 n02 m33 n01 (- m30) n05 invd)
        (gmc/inv-item m20 n05 m22 n02 m23 n01 invd)
        (gmc/inv-item m10 n10 m11 n08 m13 n06 invd)
        (gmc/inv-item m01 n08 m03 n06 (- m00) n10 invd)
        (gmc/inv-item m30 n04 m31 n02 m33 n00 invd)
        (gmc/inv-item m21 n02 m23 n00 (- m20) n04 invd)
        (gmc/inv-item m11 n07 m12 n06 (- m10) n09 invd)
        (gmc/inv-item m00 n09 m01 n07 m02 n06 invd)
        (gmc/inv-item m31 n01 m32 n00 (- m30) n03 invd)
        (gmc/inv-item m20 n03 m21 n01 m22 n00 invd))))))
g/PTranspose
(transpose
 [_]
 (Matrix44.
  m00 m10 m20 m30
  m01 m11 m21 m31
  m02 m12 m22 m32
  m03 m13 m23 m33))
g/PScale
(scale
 [_ s]
 (let [[x y z] (if (number? s) [s s s] s)]
   (g/* _ (Matrix44.
           x 0.0 0.0 0.0
           0.0 y 0.0 0.0
           0.0 0.0 z 0.0
           0.0 0.0 0.0 1.0))))
(scale
 [_ x y z]
 (g/* _ (Matrix44.
         x 0.0 0.0 0.0
         0.0 y 0.0 0.0
         0.0 0.0 z 0.0
         0.0 0.0 0.0 1.0)))

g/PRotate
(rotate
 [_ theta] (g/rotate-z _ theta))

g/PRotate3D
(rotate-x
 [_ theta]
 (let [s (Math/sin theta), c (Math/cos theta)]
   (g/* _ (Matrix44.
           1.0 0.0 0.0 0.0
           0.0 c (- s) 0.0
           0.0 s c 0.0
           0.0 0.0 0.0 1.0))))
(rotate-y
 [_ theta]
 (let [s (Math/sin theta), c (Math/cos theta)]
   (g/* _ (Matrix44.
           c 0.0 s 0.0
           0.0 1.0 0.0 0.0
           (- s) 0.0 c 0.0
           0.0 0.0 0.0 1.0))))
(rotate-z
 [_ theta]
 (let [s (Math/sin theta), c (Math/cos theta)]
   (g/* _ (Matrix44.
           c (- s) 0.0 0.0
           s c 0.0 0.0
           0.0 0.0 1.0 0.0
           0.0 0.0 0.0 1.0))))
(rotate-around-axis
 [_ [x y z] theta]
 (let [theta (- theta)
       s (Math/sin theta), c (Math/cos theta)
       sx (* s x)
       sy (* s y)
       sz (* s z)
       t (- 1.0 c)
       tx (* t x)
       ty (* t y)]
   (g/* _ (Matrix44.
           (mm/madd tx x c) (mm/madd tx y sz) (mm/msub tx z sy) 0.0
           (mm/msub tx y sz) (mm/madd ty y c) (mm/madd ty z sx) 0.0
           (mm/madd tx z sy) (mm/msub ty z sx) (mm/madd (* t z) z c) 0.0
           0.0 0.0 0.0 1.0))))

g/PTranslate
(translate
 [_ t]
 (let [[x y z] (if (number? t) [t t t] t)]
   (g/* _ (Matrix44.
           1.0 0.0 0.0 x
           0.0 1.0 0.0 y
           0.0 0.0 1.0 z
           0.0 0.0 0.0 1.0))))
(translate
 [_ x y z]
 (g/* _ (Matrix44.
         1.0 0.0 0.0 x
         0.0 1.0 0.0 y
         0.0 0.0 1.0 z
         0.0 0.0 0.0 1.0)))

g/PTransform
(transform
 [_ matrix] (g/* _ matrix))
g/PVectorTransform
(transform-vector
 [_ [x y z w]]
 (if w
   [(mm/madd x m00 y m01 z m02 w m03)
    (mm/madd x m10 y m11 z m12 w m13)
    (mm/madd x m20 y m21 z m22 w m23)
    (mm/madd x m30 y m31 z m32 w m33)]
   (thi.ng.geom.core.vector.Vec3.
    (mm/madd x m00 y m01 z m02 m03)
    (mm/madd x m10 y m11 z m12 m13)
    (mm/madd x m20 y m21 z m22 m23))))
)

#+clj (defmethod print-method Matrix32 [^Matrix32 o ^java.io.Writer w] (.write w (.toString o)))
#+clj (defmethod print-method Matrix44 [^Matrix44 o ^java.io.Writer w] (.write w (.toString o)))

(def ^:const M32
  (Matrix32.
   1.0 0.0 0.0
   0.0 1.0 0.0))

(def ^:const M44
  (Matrix44.
   1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0))

(defn matrix32
  ([] M32)
  ([[m00 m01 m02 m10 m11 m12]]
     (Matrix32. m00 m01 m02 m10 m11 m12))
  ([m00 m01 m02 m10 m11 m12]
     (Matrix32. m00 m01 m02 m10 m11 m12)))

(defn matrix44
  ([] M44)
  ([[m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33]]
     (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
  ([m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33]
     (Matrix44. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))

(defn frustum
  "Sets up a viewing frustum, shaped like a truncated pyramid with the
  camera where the tip of the pyramid would be.
  This emulates the OpenGL function glFrustum()."
  [l t r b n f]
  (let [dx (- r l)
        dy (- t b)
        dz (- f n)]
    (Matrix44.
     (/ (* 2.0 n) dx) 0.0 (mm/adddiv r l dx) 0.0
     0.0 (/ (* 2.0 n) dy) (mm/adddiv t b dy) 0.0
     0.0 0.0 (- (mm/adddiv f n dz)) (/ (mm/mul -2.0 f n) dz)
     0.0 0.0 -1.0 0.0)))

(defn frustum-bounds
  [fov aspect near]
  (let [rad (* 0.5 (m/radians fov))
        top (* near (Math/tan rad))
        right (* top aspect)]
    {:left (- right)
     :right right
     :top top
     :bottom (- top)}))

(defn ortho
  "Returns an orthographic projection matrix, in which objects are the same size no
  matter how far away or nearby they are.
  This emulates the OpenGL function glOrtho()."
  [l t r b n f]
  (let[dx (- r l)
       dy (- t b)
       dz (- f n)]
    (Matrix44.
     (/ 2.0 dx) 0.0 0.0 (- (mm/adddiv r l dx))
     0.0 (/ 2.0 dy) 0.0 (- (mm/adddiv t b dy))
     0.0 0.0 (/ -2.0 dz) (- (mm/adddiv f n dz))
     0.0 0.0 0.0 1.0)))

(defn perspective
  "Returns a perspective transform matrix, which makes far away objects appear
  smaller than nearby objects. The `aspect` argument should be the width
  divided by the height of your viewport and `fov` is the vertical angle
  of the field of view in degrees.
  This emulates the OpenGL function gluPerspective()."
  [fov aspect near far]
  (let [rad (* 0.5 (m/radians fov))
        cot (/ (Math/cos rad) (Math/sin rad))
        deltaz (- far near)
        a (/ cot aspect)
        b (- (mm/adddiv far near deltaz))
        c (/ (mm/mul -2.0 near far) deltaz)]
    (Matrix44.
     a 0.0 0.0 0.0
     0.0 cot 0.0 0.0
     0.0 0.0 b c
     0.0 0.0 -1.0 0.0)))

(defn perspective-frustum
  [fov aspect near far]
  (let [{:keys [left right top bottom]} (frustum-bounds fov aspect near)]
    (frustum left top right bottom near far)))

(defn look-at
  "Returns a matrix that puts the camera at the eye position looking
  toward the target point with the given up direction.
  This emulates the OpenGL function `gluLookAt()`."
  [eye target upvec]
  (let [[fx fy fz :as f] (g/normalize (g/- eye target))
        [sx sy sz :as s] (g/normalize (g/cross upvec f)) ;; FIXME
        [tx ty tz :as t] (g/normalize (g/cross f s))]    ;; FIXME
    (Matrix44.
     sx sy sz (- (g/dot s eye))
     tx ty tz (- (g/dot t eye))
     fx fy fz (- (g/dot f eye))
     0.0 0.0 0.0 1.0)))

;; FIXME add arity for pre-computed vpm matrix
(defn unproject
  "Takes a vec3 in screenspace, view matrix, projection matrix and
  view rect. Returns vector in model space or nil."
  [v vmat pmat {:keys [p width height]}]
  (let [x (mm/msub (- (v 0) (p 0)) (/ 2.0 width) 1.0)
        y (mm/msub (- (v 1) (p 1)) (/ 2.0 height) 1.0)
        z (mm/msub (v 2) 2.0 1.0)
        vpm (g/* pmat vmat)]
    (if (g/invert vpm)
      (let [v' (g/transform-vector vpm [x y z 1.0])]
        (if-not (zero? (v' 3))
          (g/div (v/vec3 v') (v' 3)))))))
