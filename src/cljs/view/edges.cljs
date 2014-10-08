(ns view.edges
  (:require [edge-algebra.record :refer [get-e0 get-edge]]
            [view.animation.drawing-line :refer [draw-line]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [view.delaunay :refer [run-delaunay]]))

(defn render-edges
  [canvas edge-records & {:keys [except]}]
  (let [context (.getContext canvas "2d")
        line-width 2
        scale 1
        line-color {:r 71 :g 189 :b 254}
        condition ;(if except
                    (fn [er] (and (not (:deleted er))
                                  (not= except (:index er))))
                   ; (fn [er] (not (:deleted er))))
        ]
    (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context)))
    (doseq [edge-record edge-records]
      (when (condition edge-record)
        (let [p0 (:data (get-e0 edge-record))
              p1 (:data (get-edge edge-record 2 0))]
          (draw-line context p0 p1 line-width scale line-color))))))



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


(defn edges-view
  [cursor owner opts]
  (reify
    om/IRender
    (render
     [this]
     (dom/canvas #js {:id "edges-canvas" :ref "edges-canvas"
                      :style #js {:position "absolute" :left "0px" :top "0px"
                                  :width "800px" :height "400px"
                                  :z-index 1}
                      :width "800px" :height "400px"}))

    om/IDidUpdate
    (did-update
     [_ _ _]
     (display-edges (om/get-node owner "edges-canvas") cursor)
     (print-messages cursor))))
