(ns edge-algebra.record)

;; This namespace holds some edge-record related vars
;; that we want to be able to require without introducing a dependency cycle.

(def edge-records (atom []))

(defn add-edge-record!
  [er]
  (swap! edge-records conj er))

(defn get-edge-record
  [edge-or-node]
  (let [index (:edge-record edge-or-node)]
    (@edge-records index)))


;; Edge-record accessor functions

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


(defn set-data!
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    (swap! edge-records assoc-in [er-index :edges r f :data] data)
    (get-in @edge-records [er-index :edges r f])))


(defn set-next!
  [edge next-edge]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    (swap! edge-records assoc-in [er-index :edges r f :next]
         {:r (:r next-edge) :f (:f next-edge) :edge-record (:edge-record next-edge)})
    (get-in @edge-records [er-index :edges r f])))
