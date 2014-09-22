(ns delaunay.main
  (:require [delaunay.div-conq :refer [pt delaunay]]
            [edge-algebra.state.app-mutators :refer [reset-state!]]))


(defn make-sites
  [w h how-many]
  (vec (for [_ (range how-many)]
         (pt (rand-int w) (rand-int h)))))


(defn -main
  []
  (reset-state!)
  (let [sites (make-sites 800 400 20)]
    (println sites)
    (delaunay sites)))
