(ns view.view
  (:require [cljs.core.async :refer [>! <! chan]]
            [thi.ng.geom.core.vector :refer [vec2]]
            [edge-algebra.record :refer [get-e0 get-edge]]
            [delaunay.div-conq :refer [pt delaunay]]
            [delaunay.utils.circle :refer [center-and-radius]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn draw-line
  [context p0 p1 line-width scale {:keys [r g b a]
                                     :or {a 1.0}}]
  (let [h (.-height (.-canvas context))]
    (set! (. context -strokeStyle) (str "rgba(" r "," g "," b "," a ")"))
    (set! (. context -lineWidth) line-width)
    (.beginPath context)
    (.moveTo context (* scale (.-x p0)) (- h (* scale (.-y p0))))
    (.lineTo context (* scale (.-x p1)) (- h (* scale (.-y p1))))
    (.stroke context)))

(defn draw-circle
  [context center radius line-width scale {:keys [r g b a]
                                     :or {a 1.0}}]
  (let [h (.-height (.-canvas context))]
    (set! (. context -strokeStyle) (str "rgba(" r "," g "," b "," a ")"))
    (set! (. context -lineWidth) line-width)
    (.beginPath context)
    ;; x y radius startAngle endAngle counterClockwise?:
    (.arc context (* scale (.-x center)) (- h (* scale (.-y center))) (* scale radius) 0 (* 2 Math/PI) false)
    (.stroke context)))

(defn init-canvas
  "Initialize a canvas and return it."
  [parent z-index]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "width" (.-innerWidth js/window))
    (.setAttribute canvas "height" (.-innerHeight js/window))
    (.setAttribute canvas "style" (str "z-index:" z-index
                                       "; position:absolute; left:0px; top:0px;"))
    (.appendChild parent canvas)))

(defn stop-animation
  [request-id]
  (when (pos? request-id)
    (.cancelAnimationFrame js/window request-id)))

(defn start-animation
  "Start an animation specified by update and stop?,
  both of which should be functions of elapsed-time.
  Return the request-id so that stop-animation can be called
  externally if necessary."
  [update stop?]
  (let [start-time (.now (.-performance js/window))
        request-id (atom 0)
        tick (fn tick [time]
               (let [elapsed-time (- time start-time)]
                 (if (stop? elapsed-time)
                   (stop-animation @request-id)
                   (do
                     (update elapsed-time)
                     (.requestAnimationFrame js/window tick)))))]
    (reset! request-id (.requestAnimationFrame js/window tick))
    @request-id))

(defn draw-fading-circle
  [context center radius line-width scale {:keys [r g b]} duration]
  (let [alpha (fn [elapsed-time] (* .001 (- duration elapsed-time)))
        stop? (fn [elapsed-time] (< (alpha elapsed-time) 0))
        update (fn [elapsed-time]
                 (let [square-width (+ radius radius line-width)]
                   (.clearRect context
                               (* scale (- (.-x center) (/ square-width 2)))
                               (* scale (- (.-y center) (/ square-width 2))) ;; FIX
                               (* scale square-width) (* scale square-width)))
                 (draw-circle context center radius line-width scale
                              {:r r :b b :g g :a (alpha elapsed-time)}))]
        (start-animation update stop?)))

(defn render-edges
  [context edge-records]
  (let [line-width 2
        scale 200
        line-color {:r 255 :g 0 :b 0}]
    (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context)))
    (doseq [edge-record edge-records]
      (when (not (:deleted edge-record))
        (let [p0 (:data (get-e0 edge-record))
              p1 (:data (get-edge edge-record 2 0))]
          (draw-line context p0 p1 line-width scale line-color))))))

(defn render
  [canvas center radius]
  (let [context (.getContext canvas "2d")]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))
    (draw-fading-circle context center radius 2 1 {:r 255 :g 0 :b 0} 500)))


(defn drawer
  [app-state circle-canvas & [limit]]
  (let [ch (chan 10)]
    (go-loop [index 0]
             (let [[name & args] (<! ch)]
               (condp = name
                 :make-d_edge! (swap! app-state conj (vec args)) ;; yes, a hyphen and then an underscore
                 :delete-edge! nil
                 :in-circle? (let [[center radius] (apply center-and-radius (butlast args))]
                              (render circle-canvas center radius))
                 (println "Unknown message" name)))
               (when (or (nil? limit) (< index limit))
               (recur (inc index))))
    ch))
