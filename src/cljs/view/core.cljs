(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [>! <! chan]]
            [delaunay.div-conq :as dq :refer [pt delaunay]]
            [delaunay.utils.reporting :refer [with-reporting]]
            [edge-algebra.app-state :as app-state :refer [set-cursor! wrap-with-undo]]
            [view.view :refer [render-edges]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn with-undo
  [f & [args]]
  (with-redefs [dq/make-d-edge! (wrap-with-undo dq/make-d-edge!)
                dq/delete-edge! (wrap-with-undo dq/delete-edge!)]
    (f args)))


(defn printer
  [& [limit]]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (apply prn index (<! ch))
             (when (or (nil? limit) (< index limit))
               (recur (inc index))))
    ch))

(defn run-delaunay
  []
  (let [ch (printer)
        a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)]
    (with-reporting ch (with-undo delaunay [a b c d e f])))) ;; ugh, they don't compose



(defn edges-view
  [cursor owner]
  (reify

    om/IWillMount
    (will-mount
     [_]
     (set-cursor! cursor)
     (run-delaunay))

    om/IDidUpdate
    (did-update
     [this prev-props prev-state]
     (let [context (-> (. js/document (getElementById "delaunay-canvas"))
                       (.getContext "2d"))]
       (render-edges context (:edge-records (.-value cursor)))))

    om/IRender
    (render
     [this]
     (dom/div #js {:style #js {:width "100%" :height "100%"}}
              (dom/canvas #js {:id "delaunay-canvas"
                               :width 800 :height 400})
              (dom/button #js {:width "20%" :float "left"
                               :onClick (fn [e] (do-undo))}
                          "Undo")
              (dom/button #js {:width "20%" :float "left"
                               :onClick (fn [e] (do-redo))}
                          "Redo")))))


(om/root
  edges-view
  app-state/app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
