(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan]]
            [delaunay.div-conq :as dq :refer [pt delaunay]]
            [delaunay.utils.circle :refer [center-and-radius]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!
                                                          wrap-with-undo
                                                          wrap-with-add-circle
                                                          wrap-with-clear-circles
                                                          wrap-with-add-message
                                                          replace-with-add-message
                                                          wrap-with-name-and-args-reporting
                                                          wrap-with-clear-messages
                                                          reset-state!]]
            [view.edges :refer [render-edges]]
            [view.fading-circle :refer [fading-circle-update]]
            [view.animator :refer [animator]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo do-play
                                                        do-rewind do-end]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn with-undo
  [f & [args]]
  (with-redefs [dq/make-d-edge! (wrap-with-clear-circles
                                 (wrap-with-undo
                                  dq/make-d-edge!))
                dq/delete-edge! (wrap-with-clear-circles
                                 (wrap-with-undo
                                  dq/delete-edge!))
                dq/in-circle? (wrap-with-add-circle dq/in-circle?)
                println (replace-with-add-message println)]
    (f args)))


(defn printer
  [& [limit]]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (apply prn index (<! ch))
             (when (or (nil? limit) (< index limit))
               (recur (inc index))))
    ch))

(defn make-sites
  [w h how-many]
  (vec (for [_ (range how-many)]
         (pt (rand-int w) (rand-int h)))))

#_
(defn run-delaunay
  []
  (reset-state!)
  (let [a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)]
    (with-undo delaunay [a b c d e f])))

(defn run-delaunay
  []
  (reset-state!)
  (let [
       ; sites (make-sites 800 400 20)
        sites [(pt 401 98) (pt 292 325) (pt 652 164) (pt 456 370) (pt 383 191)
               (pt 338 379) (pt 477 26) (pt 518 370) (pt 375 71) (pt 527 280)
               (pt 204 386) (pt 280 295) (pt 384 166) (pt 719 16) (pt 81 341)
               (pt 394 364) (pt 183 71) (pt 12 371) (pt 200 326) (pt 541 314)]
       ; sites [(pt 357 111) (pt 720 46) (pt 149 307) (pt 149 284)]
       ; sites [(pt 41 66) (pt 370 328) (pt 664 82) (pt 774 53)]
        ]
    (println sites)
    (with-undo delaunay sites)))


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
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "0px" :left "100px"}
                                        :onClick (fn [e] (do-rewind))}
                                   "[Rewind]")

                       (dom/button #js {:width "20%"
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "0px" :left "200px"}
                                        :onClick (fn [e] (do-undo))}
                                   "[<Step]")

                       (dom/button #js {:width "20%"
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "0px" :left "300px"}
                                        :onClick (fn [e] (do-play))}
                                   "[Play]")

                       (dom/button #js {:width "20%"
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "0px" :left "400px"}
                                        :onClick (fn [e] (do-redo))}
                                   "[Step>]")

                       (dom/button #js {:width "20%"
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "0px" :left "500px"}
                                        :onClick (fn [e] (do-end))}
                                   "[End]")

                       (dom/button #js {:width "20%"
                                        :style #js {:color "white"
                                                    :background-color "#555152"
                                                    :border "none"
                                                    :position "absolute"
                                                    :top "40px" :left "300px"}
                                        :onClick (fn [e] (run-delaunay))}
                                   "[Run]"))

              (om/build animator
                        (:circles cursor)
                        {:state {:start-time (.now (.-performance js/window))}
                         :opts {:update fading-circle-update}
                         :fn #(make-animations (.-value %))})))

    om/IDidUpdate
    (did-update
     [this prev-props prev-state]
     (let [canvas (om/get-node owner "delaunay-canvas")]
       (render-edges canvas (:edge-records (.-value cursor))))
     (println "-----------MESSAGES----------")
     (doseq [msg (:messages (.-value cursor))]
       (apply println msg))
     (println "-----------------------------"))))


(om/root
  delaunay-view
  app-state/app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
