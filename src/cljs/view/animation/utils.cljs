(ns view.animation.utils)


(defn progress
  [elapsed-time {:keys [delay duration]}]
  (/ (- elapsed-time delay) duration))

(defn lerp
  [start end progress]
  (+ start (* progress (- end start))))
