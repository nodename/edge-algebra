(ns delaunay.delaunay-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<! go go-loop chan]]
            [delaunay.div-conq :refer [pt org dest delaunay]]
            [delaunay.utils.reporting :refer [with-reporting]]))


#_
(deftest two-triangles-test
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
  [& [limit]]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (apply prn index (<! ch))
             (when (or (nil? limit) (< index limit))
               (recur (inc index))))
    ch))

(defn saver
  [v-atom]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (swap! v-atom conj (<! ch))
             (when (< index 50)
               (recur (inc index))))
    ch))


(deftest reporting-test
  (let [ch (printer)
        a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)]
    (let [[l-edge r-edge] (with-reporting ch delaunay [a b c d e f])]
      (is (= (org l-edge) (pt 0 0)))
      (is (= (dest l-edge) (pt 1 0)))
      (is (= (org r-edge) (pt 4 0)))
      (is (= (dest r-edge) (pt 2 0))))))

#_
(deftest broken-test
  (let [ch (printer 20)
        sites [(pt 38.079901003859916 47.59983051931294)
               (pt 39.73042956738154 68.19990040334532)
               (pt 68.06649970182228 76.46510955322061)
               (pt 27.878242754329108 99.65821807629467)
               (pt 38.07160264117925 64.96381721957155)
               (pt 38.08474895274715 90.45257509734398)
               (pt 84.74982605125385 22.222937179825664)
               (pt 96.4295097901255 18.27475118132984)
               (pt 72.14444693384223 92.49014504798934)
               (pt 80.88243434472668 52.29692003280921)]]
    (with-reporting ch delaunay sites)))


(deftest ok-test
  (let [ch (printer)
        sites [(pt 38.08 47.60)
               (pt 39.73 68.20)
               (pt 68.07 76.47)
               (pt 27.88 99.66)
               (pt 38.07 64.96)
               (pt 38.09 90.45)
               (pt 84.75 22.22)
               (pt 96.43 18.27)
               (pt 72.14 92.49)
               (pt 80.88 52.30)]]
    (with-reporting ch delaunay sites)))

#_
(deftest random-test
  (let [ch (printer)
        num-sites 10000
        width 10000
        height 10000
        sites (vec (for [_ (range num-sites)]
                     (pt (rand-int width) (rand-int height))))]
    (with-reporting ch delaunay sites)))
