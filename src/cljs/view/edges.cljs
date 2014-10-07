(ns view.edges
  (:require [edge-algebra.record :refer [get-e0 get-edge]]
            [view.animation.drawing-line :refer [draw-line]]))

(defn render-edges
  [canvas edge-records & {:keys [except]}]
  (println "render-edges except:" except)
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
