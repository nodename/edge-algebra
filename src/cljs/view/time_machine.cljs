(ns view.time-machine
	(:require [edge-algebra.state.app-state :refer [app-state initial-state]]
            [view.clock :refer [clock]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; Mostly copied from goya.
;; =============================================================================
;; Credits to David Nolen's Time Travel blog post.

(def app-history (atom [@app-state]))
(def app-future (atom []))
(def preview-state (atom @app-state))


(defn forget-everything []
  (reset! app-future [])
  (reset! app-history []))

;; =============================================================================

(defn update-preview []
  (reset! preview-state @app-state))


(defn show-history-preview [idx]
  (reset! preview-state (nth @app-history idx)))


(add-watch app-state :preview-watcher
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
    (reset! app-state (last @app-history))))

(defn do-redo []
  (when (redo-is-possible)
    (reset! app-state (last @app-future))
    (push-onto-undo-stack (last @app-future))
    (swap! app-future pop)))

(defn do-back
  "Go back one run."
  []
  (loop [state (do-undo)]
    (when (and (not= state initial-state)
               (undo-is-possible))
      (recur (do-undo)))))

(defn do-rewind
  "Go back to beginning of first run."
  []
  (while (do-undo)))

(defn do-next
  []
  (loop [state (do
                 (do-redo)
                 @app-state)]
    (when (and (not= state initial-state)
               (redo-is-possible))
      (recur (do
               (do-redo)
               @app-state)))))

(defn do-end []
  (while (do-redo)))

(defn do-play []
  "Play to the end of the current run."
  (do-redo)
  (let [[clock] (clock 500)]
    (go (while (and (not= (last @app-future) initial-state)
                    (do
                      (<! clock)
                      (do-redo)))))))


(defn handle-transaction [tx-data root-cursor]
  (when (= (:tag tx-data) :add-to-undo)
    (reset! app-future [])
    (let [new-state (:new-state tx-data)]
      (push-onto-undo-stack new-state))))
