(ns edge-algebra.record)

;; This namespace holds some edge-record related vars
;; that we want to be able to require without introducing a dependency cycle.

(def edge-records (atom []))

(defn next-er-index
  []
  (count @edge-records))

;; Edge-record accessor functions:

(defn get-edge-record
  [edge-or-node]
  (let [index (:edge-record edge-or-node)]
    (@edge-records index)))

(defn- get-elt
  [edge-record type r f]
  (get-in edge-record [type (mod r 4) (mod f 2)]))


(defn get-node
  [edge-record rotation f]
  (get-elt edge-record :nodes rotation f))


(defn get-edge
  [edge-record rotation f]
  (get-elt edge-record :edges rotation f))


(defn get-e0
  "Return the canonical representative edge of an edge record"
  [edge-record]
  (get-edge edge-record 0 0))

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
