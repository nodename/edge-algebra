(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [view.delaunay :refer [run-delaunay]]
            [delaunay.utils.circle :refer [center-and-radius]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!]]
            [view.edges :refer [render-edges]]
            [view.fading-circle :refer [fading-circle-update]]
            [view.animator :refer [animator]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo do-play
                                                        do-rewind do-end]]))

(enable-console-print!)


;;;; Animations  ;;;;
;;
(def palette (cycle [{:r 138 :g 155 :b 15}
                    {:r 0 :g 160 :b 176}
                    {:r 204 :g 51 :b 63}
                    {:r 235 :g 104 :b 65}
                    {:r 237 :g 201 :b 81}]))

(defn make-animation
  [in-circle-args index]
  (merge (apply center-and-radius (butlast in-circle-args))
         {:dot (last in-circle-args)
          :line-width 2
          :scale 1
          :color (nth palette index)
          :duration 250
          :delay (* 200 index)}))

(defn make-animations
  [circles]
  (map make-animation circles (range)))
;;
;;;;  Animations  ;;;;


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
     [this]

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
                         :opts {:update fading-circle-update}
                         :fn #(make-animations (.-value %))})))

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
