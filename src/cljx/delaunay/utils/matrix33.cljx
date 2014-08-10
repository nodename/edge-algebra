(ns delaunay.utils.matrix33
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

;; This namespace implements the 3x3 matrix consistently with the matrices
;; in thi.ng.geom. This is not the full implementation but just enough to
;; support calculation of determinants.

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


(deftype Matrix33
  #+clj  [^double m00 ^double m01 ^double m02
          ^double m10 ^double m11 ^double m12
          ^double m20 ^double m21 ^double m22]
  #+cljs [m00 m01 m02
          m10 m11 m12
          m20 m21 m22]
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
(#+clj count #+cljs -count [_] 9)

#+cljs ISeqable
(#+clj seq   #+cljs -seq   [_] _)

#+cljs ISeq
(#+clj more  #+cljs -rest  [_] (next _))
(#+clj first #+cljs -first [_] m00)
#+cljs INext
(#+clj next  #+cljs -next
       [_] (seq [m01 m02
                 m10 m11 m12
                 m20 m21 m22]))

#+cljs ICollection
(#+clj cons  #+cljs -conj
       [_ x]
       [m00 m01 m02
        m10 m11 m12
        m20 m21 m22 x])

#+cljs IIndexed
(#+clj nth   #+cljs -nth [_ k]
       (if (m/in-range? 0 8 k)
         (case (int k)
           0 m00  1 m01  2 m02
           3 m10  4 m11  5 m12
           6 m20  7 m21  8 m22)
         (err/illegal-arg! k)))
(#+clj nth   #+cljs -nth [_ k nf]
       (if (m/in-range? 0 8 k)
         (case (int k)
           0 m00  1 m01  2 m02
           3 m10  4 m11  5 m12
           6 m20  7 m21  8 m22)
         nf))

#+cljs IEquiv
(#+clj equiv  #+cljs -equiv [_ o]
       (if (and (sequential? o) (= 9 (count o)))
         (every? #(= (% 0) (% 1)) (map vector _ o))
         false))

#+cljs IHash
(#+clj hasheq #+cljs -hash  [_] (hash-matrix _))

#+clj (hashCode [_] (hash-coll* _))
#+clj (equals
       [_ o]
       (if (and (sequential? o) (= 9 (count o)))
         (every? #(clojure.lang.Util/equals (% 0) (% 1)) (map vector _ o))
         false))

#+clj (isEmpty
       [_] false)
#+clj (iterator
       [_] (.iterator ^java.util.Collection
                      (list m00 m01 m02
                            m10 m11 m12
                            m20 m21 m22)))
#+clj (toArray
       [_] (object-array _))
#+clj (size [_] 9)
#+clj (length [_] 9)

Object
(toString
 [_]
 (apply str (concat "[" (interpose \space _) "]")))

g/PDeterminant
(determinant
 [_]
 (reduce
  +
  [(mm/mul m00 m11 m22)
   (- (mm/mul m00 m12 m21))
   (- (mm/mul m01 m10 m22))
   (mm/mul m01 m12 m20)
   (mm/mul m02 m10 m21)
   (- (mm/mul m02 m11 m20))]))
)

#+clj (defmethod print-method Matrix33 [^Matrix33 o ^java.io.Writer w] (.write w (.toString o)))


(def ^:const M33
  (Matrix33.
   1.0 0.0 0.0
   0.0 1.0 0.0
   0.0 0.0 1.0))


(defn matrix33
  ([] M33)
  ([[m00 m01 m02 m10 m11 m12 m20 m21 m22]]
     (Matrix33. m00 m01 m02 m10 m11 m12 m20 m21 m22))
  ([m00 m01 m02 m10 m11 m12 m20 m21 m22]
     (Matrix33. m00 m01 m02 m10 m11 m12 m20 m21 m22)))
