(ns view.animation.drawing-line
  (:require [thi.ng.geom.core.vector :refer [vec2]]
            [view.animation.utils :refer [progress lerp]]))

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


(defn growing-line-update
  "Draw the line, growing, and when done, stop showing it"
  [elapsed-time canvas {:keys [start end line-width scale color duration delay] :as opts}]
  (let [context (.getContext canvas "2d")
        progress (progress elapsed-time opts)
        current-end (cond
                     (<= elapsed-time delay) start
                     (> elapsed-time (+ delay duration)) start
                     (> elapsed-time delay) (vec2 (lerp (.-x start) (.-x end) progress)
                                                  (lerp (.-y start) (.-y end) progress)))]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))
    (draw-line context start current-end line-width scale color)))


(defn shrinking-line-update
  [elapsed-time canvas {:keys [start end line-width scale color duration delay] :as opts}]
  (let [context (.getContext canvas "2d")
        progress (progress elapsed-time opts)
        current-end (cond
                     (<= elapsed-time delay) end
                     (> elapsed-time (+ delay duration)) start
                     (> elapsed-time delay) (vec2 (lerp (.-x start) (.-x end) (- 1 progress))
                                                  (lerp (.-y start) (.-y end) (- 1 progress))))]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))
    (draw-line context start current-end line-width scale color)))
