(ns edge-algebra.cheat-codes
(:require [edge-algebra.app-state :refer [app-state]]
          [edge-algebra.edge :refer [o-next sym]]))


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
