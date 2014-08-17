(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [>! <! chan]]
            [delaunay.div-conq :refer [pt delaunay with-reporting]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!]]
            [view.view :refer [render-edges]]
            [view.time-machine :as time-machine])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(reset! time-machine/preview-state @app-state/edge-records)


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
       (with-reporting ch delaunay [a b c d e f])))


(defn tx-listener [tx-data root-cursor]
  (time-machine/handle-transaction tx-data root-cursor))


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
       (render-edges context (.-value cursor))))

    om/IRender
    (render
     [this]
     (dom/canvas #js {:id "delaunay-canvas"
                      :width 800 :height 400}))))


(om/root
  edges-view
  app-state/edge-records
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen tx-listener})
