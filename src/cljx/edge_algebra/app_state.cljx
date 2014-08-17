(ns edge-algebra.app-state
  #+cljs (:require [om.core :as om :include-macros true]))

(def edge-records (atom []))

#+cljs
(def cursor (atom nil))

#+cljs
(defn set-cursor!
  [c]
  (println "Setting cursor to " c)
  (reset! cursor c))

(defn next-er-index
  []
  (count @edge-records))

(defn get-edge-record
  [edge-or-node]
  (let [er-index (:edge-record edge-or-node)]
    (@edge-records er-index)))

;; Mutators:

(defn add-edge-record!
  [er]
  #+clj (swap! edge-records conj er)
  #+cljs (om/transact! @cursor #(conj % er)))

(defn remove-edge-record!
  "Mark edge's edge record as deleted. We don't really delete it
  because edge records are referred to by their indices in @edge-records."
  [edge]
  (let [er-index (:edge-record edge)]
    #+clj (swap! edge-records assoc-in [er-index :deleted] true)
    #+cljs (om/transact! @cursor [er-index :deleted] (constantly true))))


(defn set-data!
  "Set edge's data. Return the updated edge."
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)]
    #+clj (swap! edge-records assoc-in [er-index :edges r f :data] data)
    #+cljs (om/transact! @cursor [er-index :edges r f :data] (constantly data))
    (get-in @edge-records [er-index :edges r f])))


(defn set-next!
  "Set edge's next. Return the updated edge."
  [edge next-edge]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)
        next {:r (:r next-edge)
              :f (:f next-edge)
              :edge-record (:edge-record next-edge)}]
    #+clj (swap! edge-records assoc-in [er-index :edges r f :next] next)
    #+cljs (om/transact! @cursor [er-index :edges r f :next] (constantly next))
    (get-in @edge-records [er-index :edges r f])))
