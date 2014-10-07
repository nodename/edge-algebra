(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! alts!]]
            [view.delaunay :refer [run-delaunay]]
            [edge-algebra.state.app-state :refer [app-state set-cursor!]]
            [view.edges :refer [render-edges]]
            [view.animator :refer [animator]]
            [view.controls :refer [controls-view]]
            [view.time-machine :as time-machine :refer [handle-transaction]]))

(enable-console-print!)


(defn display-edges
  [canvas cursor]
  (render-edges canvas (:edge-records (om/value cursor))
                :except (:er-index (:current-edge-record (om/value cursor)))))


(defn print-messages
  [cursor]
  (println "-----------MESSAGES----------")
  (doseq [msg (:messages (om/value cursor))]
    (apply println msg))
  (println "-----------------------------"))


(defn delaunay-view
  [cursor owner opts]
  (reify
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


              (dom/canvas #js {:id "edges-canvas" :ref "edges-canvas"
                               :style #js {:position "absolute" :left "0px" :top "0px"
                                           :width "800px" :height "400px"
                                           :z-index 1}
                               :width "800px" :height "400px"})

              (om/build controls-view
                        nil)

              (om/build animator
                        cursor
                        {:state {:start-time (.now (.-performance js/window))}})))

    om/IDidUpdate
    (did-update
     [_ _ _]
     (display-edges (om/get-node owner "edges-canvas") cursor)
     (print-messages cursor))))


(om/root
  delaunay-view
  app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
