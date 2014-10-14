;; <img src="Marge_small.png" />

(ns edge-algebra.cheat-codes
(:require [edge-algebra.state.app-state :refer [app-state]]
          [edge-algebra.edge :refer [o-next sym]]))

;; ## Some debugging aids

;; The Delaunay application stores the origin and dest coordinates
;; of an edge in its own and its symmetric edge's data fields respectively.
;; An edge-algebra function can report these data while plausibly
;; denying knowledge of what they represent.
(defn edge-info
  [edge]
  (str (:edge-record edge) " " (:r edge) " " (:f edge) " "
       (:data edge) "->" (:data (sym edge))))


(defn show-all-edge-records
  [app-state]
  (println "All edge records:")
  (doseq [er (:edge-records @app-state)]
    (println "ER" (:index er) ":")
    (doseq [pair (:edges er)]
      (doseq [edge pair]
        (println "Edge:" (edge-info edge) "next:"(edge-info (o-next edge)))))))
