(ns thi.ng.common.data.intervaltree
 #+cljs (:require-macros [thi.ng.macromath.core :as mm])
 (:require
  [thi.ng.common.math.core :as m]
  [clojure.core.reducers :as r]
  #+clj [thi.ng.macromath.core :as mm]))

(defprotocol PIntervalTree
  (add-interval [_ i x])
  (query-point [_ x acc])
  (query-interval [_ i acc]))

(defn sort-min
  [a b]
  (let [c (compare (nth a 0) (nth b 0))]
    (if (zero? c) (compare a b) c)))

(defn sort-max
  [a b]
  (let [c (compare (nth b 1) (nth a 1))]
    (if (zero? c) (compare b a) c)))

(deftype IntervalNode
    [median
     #+clj ^:unsynchronized-mutable left
     #+cljs ^:mutable left
     #+clj ^:unsynchronized-mutable right
     #+cljs ^:mutable right
     #+clj ^:unsynchronized-mutable c-left
     #+cljs ^:mutable c-left
     #+clj ^:unsynchronized-mutable c-right
     #+cljs ^:mutable c-right
     ]
  PIntervalTree
  (add-interval [_ [il ih :as i] val]
    (cond
     (< ih median)
     (if left
       (add-interval left i val)
       (let [val #{val}]
         (set! left (IntervalNode. (mm/addm il ih 0.5) nil nil
                                   (sorted-map-by sort-min i val)
                                   (sorted-map-by sort-max i val)))))

     (> il median)
     (if right
       (add-interval right i val)
       (let [val #{val}]
         (set! right (IntervalNode. (mm/addm il ih 0.5) nil nil
                                    (sorted-map-by sort-min i val)
                                    (sorted-map-by sort-max i val)))))

     :else (do
             (set! c-left (update-in c-left [i] (fnil conj #{}) val))
             (set! c-right (update-in c-right [i] (fnil conj #{}) val))))
    _)
  (query-point
    [_ x acc]
    (let [acc (if (m/delta= x median)
                (into acc (mapcat val c-left))
                (if (< x median)
                  (->> c-left (r/take-while #(<= (nth (key %) 0) x)) (r/mapcat val) (into acc))
                  (->> c-right (r/take-while #(>= (nth (key %) 1) x)) (r/mapcat val) (into acc))))
          acc (if (and left (< x median))
                (query-point left x acc)
                acc)
          acc (if (and right (> x median))
                (query-point right x acc)
                acc)]
      acc))
  (query-interval
    [_ [a b :as i] acc]
    (let [acc (->> c-left
                   (r/filter #(let [k (key %)] (and (<= (nth k 0) b) (>= (nth k 1) a))))
                   (r/mapcat val)
                   (into acc))
          acc (if (and left (< a median))
                (query-interval left i acc)
                acc)
          acc (if (and right (> b median))
                (query-interval right i acc)
                acc)]
      acc))
  Object
  (toString
    [_]
    (str ":median " median
         ", :left " (pr-str left)
         ", :right " (pr-str right)
         ", :c-left " (pr-str c-left)
         ", :c-right " (pr-str c-right)
         )))

(defn interval-tree
  ([x]
     (IntervalNode. x nil nil (sorted-map-by sort-min) (sorted-map-by sort-max)))
  ([x coll]
     (reduce (fn [t [k v]] (add-interval t k v)) (interval-tree x) coll)))
