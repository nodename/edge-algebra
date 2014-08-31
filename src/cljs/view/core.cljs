(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan]]
            [delaunay.div-conq :as dq :refer [pt delaunay]]
            [delaunay.utils.circle :refer [center-and-radius]]
            [delaunay.utils.reporting :refer [with-reporting]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!
                                                          wrap-with-undo
                                                          wrap-with-add-circle
                                                          wrap-with-clear-circles]]
            [view.edges :refer [render-edges]]
            [view.fading-circle :refer [fading-circle-update]]
            [view.animator :refer [animator]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn with-undo
  [f & [args]]
  (with-redefs [dq/make-d-edge! (wrap-with-clear-circles (wrap-with-undo dq/make-d-edge!))
                dq/delete-edge! (wrap-with-clear-circles (wrap-with-undo dq/delete-edge!))
                dq/in-circle? (wrap-with-add-circle dq/in-circle?)]
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



(def colors (cycle [{:r 138 :g 155 :b 15}
                    {:r 0 :g 160 :b 176}
                    {:r 204 :g 51 :b 63}
                    {:r 235 :g 104 :b 65}
                    {:r 237 :g 201 :b 81}]))


(defn make-animation
  [circle index]
  (merge (apply center-and-radius (butlast circle))
         {:line-width 2
          :scale 200
          :duration 500
          :color (nth colors index)
          :delay (* 200 index)}))

(defn make-animations
  [circles]
  (map make-animation circles (range)))



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


              (dom/button #js {:width "20%"
                               :style #js {:position "absolute" :top "420px" :left "20px"}
                               :onClick (fn [e] (do-undo))}
                          "[__<__]")

              (dom/button #js {:width "20%"
                               :style #js {:position "absolute" :top "420px" :left "120px"}
                               :onClick (fn [e] (do-redo))}
                          "[__>__]")

              (dom/button #js {:width "20%"
                               :style #js {:position "absolute" :top "420px" :left "220px"}
                               :onClick (fn [e] (println "hello"))}
                          "Hello?")

              (println "circles:" (:circles cursor))

              (om/build animator
                        (:circles cursor)
                        {:state {:start-time (.now (.-performance js/window))}
                         :opts {:update fading-circle-update}
                         :fn #(make-animations (.-value %))})))


    om/IDidUpdate
    (did-update
     [this prev-props prev-state]
     (let [canvas (om/get-node owner "delaunay-canvas")]
       (render-edges canvas (:edge-records (.-value cursor)))))))


(om/root
  delaunay-view
  app-state/app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
