(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! alts!]]
            [view.delaunay :refer [run-delaunay]]
            [edge-algebra.state.app-state :refer [app-state set-cursor!]]
            [view.animator :refer [animator]]
            [view.edges :refer [edges-view]]
            [view.controls :refer [controls-view]]
            [view.time-machine :as time-machine :refer [handle-transaction]]))

(enable-console-print!)


(defn delaunay-view
  [cursor owner opts]
  (reify
    om/IInitState
    (init-state
     [_]
     {:animation-done-ch (chan)})

    om/IWillMount
    (will-mount
     [_]
     (set-cursor! cursor)
     (run-delaunay))

    om/IRender
    (render
     [this]

     (dom/div #js {:width "800px" :height "500px"
                   :style #js {:width "800px" :height "500px"}}

              (om/build edges-view
                        cursor)

              (om/build controls-view
                        nil
                        {:opts {:animation-done-ch (om/get-state owner :animation-done-ch)}})

              (om/build animator
                        cursor
                        {:opts {:animation-done-ch (om/get-state owner :animation-done-ch)}
                         :state {:start-time (.now (.-performance js/window))}})))))


(om/root
  delaunay-view
  app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
