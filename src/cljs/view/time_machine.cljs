(ns view.time-machine
	(:require [edge-algebra.state.app-state :refer [app-state initial-state]]
            [cljs.core.async :refer [<!]])
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


(defn next-state
  []
  (when (redo-is-possible)
    (last @app-future)))


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


(defn init-play
  "Start a process that will play to the next end of a run
  as long as clock ticks are received."
  [clock notify-done]
  (go
   (loop [status :starting]
     (<! clock)
     ;; When starting up at the end of a run,
     ;; step forward:
     (when (and (= status :starting)
                (= (next-state) initial-state))
       (do-redo))
     (if (and (redo-is-possible)
              (not= (next-state) initial-state))
       ;; I have a next state to continue to;
       ;; keep going:
       (do
         (<! clock)
         (do-redo)
         (recur :continuing))
       ;; otherwise, request the clock to stop,
       ;; swallow a final clock tick (not sure why),
       ;; and return to start:
       (do
         (notify-done)
         (<! clock)
         (recur :starting))))))


(defn handle-transaction [tx-data root-cursor]
  (when (= (:tag tx-data) :add-to-undo)
    (reset! app-future [])
    (let [new-state (:new-state tx-data)]
      (push-onto-undo-stack new-state))))
