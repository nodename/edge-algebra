(ns view.controls
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async
             :refer [put! chan <! alts!]]
            [view.delaunay
             :refer [run-delaunay]]
            [view.clock
             :refer [clock]]
            [view.time-machine :as time-machine
             :refer [init-play
                     do-undo do-redo
                     do-back do-next
                     do-rewind do-end]]))

(defn button-style
  [top left]
  (clj->js {:color "white"
            :background-color "#555152"
            :border "none"
            :position "absolute"
            :top (str top "px")
            :left (str left "px")
            :text-align "center"}))


(defn start-play
  [owner]
  (let [play (om/get-state owner :play)]
    (put! play :play)
    (om/set-state! owner :playing true)))


(defn pause-play
  [owner]
  (let [pause (om/get-state owner :pause)]
    (put! pause :pause)
    (om/set-state! owner :playing false)))


(defn controls-view
  [_ owner _]
  (reify
    om/IInitState
    (init-state
     [_]
     (let [[tick play pause] (clock 200 :run-at-start false)]
      {:tick tick
       :play play
       :pause pause
       :playing false}))

    om/IWillMount
    (will-mount
     [_]
     (init-play (om/get-state owner :tick) #(pause-play owner)))

    om/IRenderState
    (render-state
     [this {:keys [playing]}]
     (dom/div #js {:style #js {:position "relative" :top "420px" :left "60px"}}

              (dom/button #js {:width "20%"
                               :style (button-style 0 0)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-rewind))}
                          "[<<<Rewind]")

              (dom/button #js {:width "20%"
                               :style (button-style 0 100)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-back))}
                          "[<<Back]")

              (dom/button #js {:width "20%"
                               :style (button-style 0 200)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-undo))}
                          "[<Step]")

              (if playing
                (dom/button #js {:width "20%"
                                 :style (button-style 0 300)
                                 :onClick (fn [e] (pause-play owner))}
                            "[  ||  ]")
                (dom/button #js {:width "20%"
                                 :style (button-style 0 300)
                                 :onClick (fn [e] (start-play owner))}
                            "[Play]"))

              (dom/button #js {:width "20%"
                               :style (button-style 0 400)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-redo))}
                          "[Step>]")

              (dom/button #js {:width "20%"
                               :style (button-style 0 500)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-next))}
                          "[Next>>]")

              (dom/button #js {:width "20%"
                               :style (button-style 0 600)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (do-end))}
                          "[End>>>]")

              (dom/button #js {:width "20%"
                               :style (button-style 40 300)
                               :onClick (fn [e]
                                          (pause-play owner)
                                          (run-delaunay))}
                          "[Run]")))))
