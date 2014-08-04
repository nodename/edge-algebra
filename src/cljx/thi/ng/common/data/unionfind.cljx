(ns thi.ng.common.data.unionfind)

(defprotocol PUnionFind
  (add-single [_ p])
  (canonical [_ p])
  (disjoint-components [_])
  (component-for [_ p])
  (union [_ [p q]] [_ p q])
  (unified? [_ p q]))

(defprotocol PUnionFind
  (add-single [_ p])
  (canonical [_ p])
  (disjoint-components [_])
  (component-for [_ p])
  (union [_ [p q]] [_ p q])
  (unified? [_ p q]))

(deftype DisjointSet [index components]
  PUnionFind
  (canonical [_ p]
    (or (index p) (when (components p) p)))
  (unified? [_ p q]
    (= (index p p) (index q q)))
  (component-for [_ p]
    (components (canonical _ p)))
  (disjoint-components [_]
    (vals components))
  (add-single
    [_ p]
    (if (canonical _ p)
      _
      (DisjointSet. (assoc index p p) (assoc components p [p]))))
  (union [_ [p q]]
    (union _ p q))
  (union [_ p q]
    (let [canon1 (index p p)
          canon2 (index q q)]
      (if (= canon1 canon2)
        _
        (let [comp1 (or (components canon1) [canon1])
              comp2 (or (components canon2) [canon2])
              [canon1 canon2 comp1 comp2] (if (<= (count comp1) (count comp2))
                                            [canon1 canon2 comp1 comp2]
                                            [canon2 canon1 comp2 comp1])]
          (DisjointSet.
           (into index (for [item comp1] [item canon2]))
           (-> components
               (dissoc canon1)
               (assoc canon2 (into comp2 comp1))))))))
  Object
  (toString [_] (pr-str {:index index :components components})))

(defn disjoint-set
  ([] (DisjointSet. {} {}))
  ([xs] (reduce union (DisjointSet. {} {}) xs)))
