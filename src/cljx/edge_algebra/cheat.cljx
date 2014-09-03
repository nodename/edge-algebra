(ns edge-algebra.cheat
(:require [edge-algebra.app-state :refer [app-state]]
          [edge-algebra.edge :refer [o-next sym]]))


(defn verts
  [edge]
  (str (:data edge) "->" (:data (sym edge))))


(defn show-all-edge-records
  [app-state]
  (doseq [er (:edge-records @app-state)]
    (doseq [e (:edges er)]
      (println (verts e) (verts (o-next e))))))
