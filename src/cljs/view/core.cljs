(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [view.delaunay :refer [run-delaunay]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!]]
            [view.edges :refer [render-edges]]
            [view.fading-circle :refer [fading-circle-update]]
            [view.animator :refer [animator]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo do-play
                                                        do-rewind do-end]]))

(enable-console-print!)


(defn button-style
  [top left]
  (clj->js {:color "white"
            :background-color "#555152"
            :border "none"
            :position "absolute"
            :top (str top "px")
            :left (str left "px")}))


(defn display-edges
  [owner cursor]
  (let [canvas (om/get-node owner "delaunay-canvas")]
    (render-edges canvas (:edge-records (.-value cursor)))))

(defn print-messages
  [cursor]
  (println "-----------MESSAGES----------")
  (doseq [msg (:messages (.-value cursor))]
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
     [_]

     (dom/div #js {:width "800px" :height "500px"
                   :style #js {:width "800px" :height "500px"}}


              (dom/canvas #js {:id "delaunay-canvas" :ref "delaunay-canvas"
                               :style #js {:position "absolute" :left "0px" :top "0px"
                                           :width "800px" :height "400px"
                                           :z-index 1}
                               :width "800px" :height "400px"})


              (dom/div #js {:style #js {:position "relative" :top "420px" :left "60px"}}

                       (dom/button #js {:width "20%"
                                        :style (button-style 0 100)
                                        :onClick (fn [e] (do-rewind))}
                                   "[Rewind]")

                       (dom/button #js {:width "20%"
                                        :style (button-style 0 200)
                                        :onClick (fn [e] (do-undo))}
                                   "[<Step]")

                       (dom/button #js {:width "20%"
                                        :style (button-style 0 300)
                                        :onClick (fn [e] (do-play))}
                                   "[Play]")

                       (dom/button #js {:width "20%"
                                        :style (button-style 0 400)
                                        :onClick (fn [e] (do-redo))}
                                   "[Step>]")

                       (dom/button #js {:width "20%"
                                        :style (button-style 0 500)
                                        :onClick (fn [e] (do-end))}
                                   "[End]")

                       (dom/button #js {:width "20%"
                                        :style (button-style 40 300)
                                        :onClick (fn [e] (run-delaunay))}
                                   "[Run]"))

              (om/build animator
                        (:circles cursor)
                        {:state {:start-time (.now (.-performance js/window))}
                         :opts {:update fading-circle-update}})))

    om/IDidUpdate
    (did-update
     [_ _ _]
     (display-edges owner cursor)
     (print-messages cursor))))


(om/root
  delaunay-view
  app-state/app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
