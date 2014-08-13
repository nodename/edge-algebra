(ns view.view
  (:require [thi.ng.geom.core.vector :refer [vec2]]))

(enable-console-print!)

(defn draw-line
  [context p0 p1 line-width {:keys [r g b]}]
  (set! (. context -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. context -lineWidth) line-width)
  (.beginPath context)
  (.moveTo context (.-x p0) (.-y p0))
  (.lineTo context (.-x p1) (.-y p1))
  (.stroke context))

(defn draw-circle
  [context center radius line-width {:keys [r g b a]
                                     :or {a 1.0}}]
  (set! (. context -strokeStyle) (str "rgba(" r "," g "," b "," a ")"))
  (set! (. context -lineWidth) line-width)
  (.beginPath context)
  ;; x y radius startAngle endAngle counterClockwise:
  (.arc context (.-x center) (.-y center) radius 0 (* 2 Math/PI) false)
  (.stroke context))

(defn init-canvas
  "Initialize a canvas and return it."
  [z-index]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "width" (.-innerWidth js/window))
    (.setAttribute canvas "height" (.-innerHeight js/window))
    (.setAttribute canvas "style" (str "z-index:" z-index
                                       "; position:absolute; left:0px; top:0px;"))
    (.appendChild (.-body js/document) canvas)))

(defn stop-animation
  [request-id]
  (when (pos? request-id)
    (.cancelAnimationFrame js/window request-id)))

(defn start-animation
  [update stop-condition]
  (let [start-time (.now (.-performance js/window))
        request-id (atom 0)
        tick (fn tick [time]
               (let [elapsed-time (- time start-time)]
                 (if (stop-condition elapsed-time)
                   (stop-animation @request-id)
                   (do
                     (update elapsed-time)
                     (.requestAnimationFrame js/window tick)))))]
    (reset! request-id (.requestAnimationFrame js/window tick))
    @request-id))

(defn draw-fading-circle
  [context center radius line-width {:keys [r g b]} duration]
  (let [alpha (fn [elapsed-time] (* .001 (- duration elapsed-time)))
        stop-condition (fn [elapsed-time] (< (alpha elapsed-time) 0))
        update (fn [elapsed-time]
                 (let [rect-width (+ radius radius line-width)]
                   (.clearRect context
                               (- (.-x center) (/ rect-width 2))
                               (- (.-y center) (/ rect-width 2))
                               rect-width rect-width))
                 (draw-circle context center radius line-width
                              {:r r :b b :g g :a (alpha elapsed-time)}))]
        (start-animation update stop-condition)))


(defn render
  [canvas center]
  (let [context (.getContext canvas "2d")]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))
    (draw-fading-circle context center 50 2 {:r 255 :g 0 :b 0} 500)))



(defn main
  []
  (let [canvas1 (init-canvas 1)
        canvas2 (init-canvas 2)]
    (render canvas1 (vec2 200 200))
    (render canvas2 (vec2 300 300))
    ))

