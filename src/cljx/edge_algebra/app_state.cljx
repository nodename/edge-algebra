(ns edge-algebra.app-state)

(def initial-state {:edge-records []
                    :circles []
                    :messages []})

(def app-state (atom initial-state))

(defn next-er-index
  []
  (count (:edge-records @app-state)))

(defn get-edge-record
  [edge-or-node]
  (let [er-index (:edge-record edge-or-node)]
    ((:edge-records @app-state) er-index)))

