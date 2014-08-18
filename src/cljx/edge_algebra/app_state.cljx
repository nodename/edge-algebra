(ns edge-algebra.app-state
  #+cljs (:require [om.core :as om :include-macros true]))

(def app-state (atom {:edge-records []}))

#+cljs
(def cursor (atom nil))

#+cljs
(defn set-cursor!
  [c]
  (reset! cursor c))

(defn next-er-index
  []
  (count (:edge-records @app-state)))

(defn get-edge-record
  [edge-or-node]
  (let [er-index (:edge-record edge-or-node)]
    ((:edge-records @app-state) er-index)))

;; Mutators:

#+cljs
(defn add-marker-transaction!
  "Add a marker transaction for the current cursor state."
  []
  (om/transact! @cursor [] (constantly @app-state) :add-to-undo))


(defn add-edge-record!
  [er]
  #+clj (swap! app-state update-in [:edge-records] conj er)
  #+cljs (om/transact! @cursor [:edge-records] #(conj % er)))


(defn remove-edge-record!
  "Mark edge's edge record as deleted. We don't really delete it
  because edge records are referred to by their indices in :edge-records."
  [edge]
  (let [er-index (:edge-record edge)]
    #+clj (swap! app-state assoc-in [:edge-records er-index :deleted] true)
    #+cljs (om/transact! @cursor [:edge-records er-index :deleted]
                         (constantly true)
                         :add-to-undo)))


(defn set-data!
  "Set edge's data. Return the updated edge."
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    #+clj (swap! app-state assoc-in [:edge-records er-index :edges r f :data] data)
    #+cljs (om/transact! @cursor [:edge-records er-index :edges r f :data]
                         (constantly data))
    (get-in @app-state [:edge-records er-index :edges r f])))


(defn set-next!
  "Set edge's next. Return the updated edge."
  [edge next-edge]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)
        next {:r (:r next-edge)
              :f (:f next-edge)
              :edge-record (:edge-record next-edge)}]
    #+clj (swap! app-state assoc-in [:edge-records er-index :edges r f :next] next)
    #+cljs (om/transact! @cursor [:edge-records er-index :edges r f :next]
                         (constantly next))
    (get-in @app-state [:edge-records er-index :edges r f])))
