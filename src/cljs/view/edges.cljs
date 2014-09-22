(ns view.edges
  (:require [edge-algebra.record :refer [get-e0 get-edge]]))

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

(defn render-edges
  [canvas edge-records]
  (let [context (.getContext canvas "2d")
        line-width 2
        scale 1
        line-color {:r 71 :g 189 :b 254}]
    (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context)))
    (doseq [edge-record edge-records]
      (when (not (:deleted edge-record))
        (let [p0 (:data (get-e0 edge-record))
              p1 (:data (get-edge edge-record 2 0))]
          (draw-line context p0 p1 line-width scale line-color))))))
