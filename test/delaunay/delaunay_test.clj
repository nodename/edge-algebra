(ns delaunay.delaunay-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<! go-loop chan]]
            [delaunay.div-conq :refer [pt org dest delaunay with-reporting]]))


(deftest two-triangles-test
  []
  (let [a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)
        [l-edge r-edge] (delaunay [a b c d e f])]
    (is (= (org l-edge) (pt 0 0)))
    (is (= (dest l-edge) (pt 1 0)))
    (is (= (org r-edge) (pt 4 0)))
    (is (= (dest r-edge) (pt 2 0)))))

(defn printer
  []
  (let [ch (chan 10)]
    (go-loop []
             (apply prn (<! ch))
             (recur))
    ch))

(deftest reporting-test
  []
  (let [ch (printer)
        a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)]
    (let [[l-edge r-edge] (with-reporting ch delaunay [a b c d e f])])))
