(ns edge-algebra.app-state
  #+cljs (:require [om.core :as om :include-macros true]))

(def initial-state {:edge-records []
                    :circles []
                    :messages []})

(def app-state (atom initial-state))


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

