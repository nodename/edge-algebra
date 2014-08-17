(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as omdom :include-macros true]
            [cljs.core.async :refer [>! <! chan]]
            [delaunay.div-conq :refer [pt org dest delaunay with-reporting]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!]]
            [view.timemachine :as timemachine])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)


(reset! timemachine/preview-state @app-state/edge-records)


(defn printer
  [& [limit]]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (apply prn index (<! ch))
             (when (or (nil? limit) (< index limit))
               (recur (inc index))))
    ch))


(defn tx-listener [tx-data root-cursor]
  (timemachine/handle-transaction tx-data root-cursor))

(defn edges-view
  [cursor owner]
  (reify
    om/IInitState
    (init-state
    [_]
     {})



    om/IWillMount
    (will-mount
     [_]
     (when cursor
       (set-cursor! cursor))

     (let [ch (printer)
           a (pt 0 0)
           b (pt 0 1)
           c (pt 1 0)
           d (pt 2 0)
           e (pt 3 1)
           f (pt 4 0)]
       (with-reporting ch delaunay [a b c d e f])))



    om/IRenderState
    (render-state
     [this state]
     (omdom/h1 #js {:className "app-title"}
               "Hello Om"
               (omdom/h6 #js {:className "app-subtitle"}
                         "Subtitle")))))

(om/root
  edges-view
  app-state/edge-records
  {:target (. js/document (getElementById "title"))
   ;; :tx-listen is a callback that will be invoked
   ;; any time the application state transitions:
   :tx-listen tx-listener})
