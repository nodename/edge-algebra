(ns view.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [>! <! chan alts!]]
            [delaunay.div-conq :as dq :refer [pt delaunay]]
            [delaunay.utils.circle :refer [center-and-radius]]
            [delaunay.utils.reporting :refer [with-reporting]]
            [edge-algebra.app-state :as app-state :refer [set-cursor!
                                                          wrap-with-undo
                                                          wrap-with-add-circle
                                                          wrap-with-clear-circles]]
            [view.view :refer [render-edges]]
            [view.animator :refer [animator]]
            [view.time-machine :as time-machine :refer [handle-transaction
                                                        do-undo do-redo]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-circle
  [context center radius line-width scale {:keys [r g b a]
                                           :or {a 1.0}}]
  (let [h (.-height (.-canvas context))
        center-x (* scale (:x center))
        center-y (- h (* scale (:y center)))
        radius (* scale radius)]
    (set! (. context -strokeStyle) (str "rgba(" r "," g "," b "," a ")"))
    (set! (. context -lineWidth) line-width)
    (.beginPath context)
    ;; x y radius startAngle endAngle counterClockwise?:
    (.arc context center-x center-y radius 0 (* 2 Math/PI) false)
    (.stroke context)))

(defn alpha
  [elapsed-time {:keys [delay duration]}]
  #_(println "alpha: delay:" delay "duration:" duration "elapsed-time:" elapsed-time)
  (+ 1 (/ (- delay elapsed-time) duration)))

(defn fading-circle-stop?
  [elapsed-time opts]
  (let [a (alpha elapsed-time opts)]
   #_ (println "stop?: alpha" a)
    (<= a 0)))

(defn fading-circle-update
  [elapsed-time canvas {:keys [center radius line-width scale color delay] :as opts}]
  (let [context (.getContext canvas "2d")
        a (alpha elapsed-time opts)]
    (when (>= elapsed-time delay)
      (.clearRect context 0 0 (.-width canvas) (.-height canvas))
      (draw-circle context center radius line-width scale
                   (merge color {:a a})))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def colors (cycle [{:r 255 :g 0 :b 0}
                    {:r 0 :g 255 :b 0}
                    {:r 0 :g 0 :b 255}])) ;; TODO make this a spectrum


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



(defn edges-view
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
                               :onClick (fn [e]
                                          (println "undo")
                                          (do-undo))}
                          "Undo")
              (dom/button #js {:width "20%"
                               :style #js {:position "absolute" :top "420px" :left "80px"}
                               :onClick (fn [e]
                                          (println "redo")
                                          (do-redo))}
                          "Redo")
              (dom/button #js {:width "20%"
                               :style #js {:position "absolute" :top "420px" :left "140px"}
                               :onClick (fn [e]
                                          (println "hello"))}
                          "Hello?")

              (println "circles:" (:circles cursor))

              (let [m {:state {:start-time (.now (.-performance js/window))}
                       :opts {:stop? fading-circle-stop?
                              :update fading-circle-update}
                       :fn #(make-animations (.-value %))}]

                (om/build animator (:circles cursor) m))))


    om/IDidUpdate
    (did-update
     [this prev-props prev-state]
     (let [canvas (om/get-node owner "delaunay-canvas")]
       (render-edges canvas (:edge-records (.-value cursor)))))))




(om/root
  edges-view
  app-state/app-state
  {:target (. js/document (getElementById "delaunay"))
   :tx-listen handle-transaction})
