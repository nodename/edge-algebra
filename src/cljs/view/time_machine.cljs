(ns view.time-machine
	(:require [edge-algebra.app-state :refer [edge-records]]))

;; Copied from goya.
;; =============================================================================
;; Credits to David Nolen's Time Travel blog post.

(def app-history (atom [@edge-records]))
(def app-future (atom []))
(def preview-state (atom {}))


(defn forget-everything []
  (reset! app-future [])
  (reset! app-history []))

;; =============================================================================

(defn update-preview []
  (reset! preview-state @edge-records))


(defn show-history-preview [idx]
  (reset! preview-state (nth @app-history idx)))


(add-watch edge-records :preview-watcher
  (fn [_ _ _ _] (update-preview)))



(defn undo-is-possible []
  (> (count @app-history) 1))

(defn redo-is-possible []
  (> (count @app-future) 0))


(defn push-onto-undo-stack [new-state]
  (let [old-watchable-app-state (last @app-history)]
    (when-not (= old-watchable-app-state new-state)
      (swap! app-history conj new-state))))


(defn do-undo []
  (when (undo-is-possible)
    (swap! app-future conj (last @app-history))
    (swap! app-history pop)
    (reset! edge-records (last @app-history))))

(defn do-redo []
  (when (redo-is-possible)
    (reset! edge-records (last @app-future))
    (push-onto-undo-stack (last @app-future))
    (swap! app-future pop)))


(defn handle-transaction [tx-data root-cursor]
  (when (= (:tag tx-data) :add-to-undo)
    (reset! app-future [])
    (let [new-state (get-in (:new-state tx-data) [:edge-records])]
      (push-onto-undo-stack new-state))))
