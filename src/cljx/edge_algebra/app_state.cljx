(ns edge-algebra.app-state)

(def edge-records (atom []))

(defn next-er-index
  []
  (count @edge-records))

(defn get-edge-record
  [edge-or-node]
  (let [index (:edge-record edge-or-node)]
    (@edge-records index)))

;; Mutators:

(defn add-edge-record!
  [er]
  (swap! edge-records conj er))

(defn remove-edge-record!
  "Mark edge's edge-record as deleted."
  [edge]
  (swap! edge-records assoc-in [(:edge-record edge) :deleted] true))


(defn set-data!
  "Set edge's data. Return the updated edge."
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    (swap! edge-records assoc-in [er-index :edges r f :data] data)
    (get-in @edge-records [er-index :edges r f])))


(defn set-next!
  "Set edge's next. Return the updated edge."
  [edge next-edge]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    (swap! edge-records assoc-in [er-index :edges r f :next]
         {:r (:r next-edge) :f (:f next-edge) :edge-record (:edge-record next-edge)})
    (get-in @edge-records [er-index :edges r f])))
