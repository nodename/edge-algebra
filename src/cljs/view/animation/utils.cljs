(ns view.animation.utils)


(defn progress
  "Linear progress from 0 (at elapsed_time = delay)
  to 1 (at elapsed_time = delay + duration)"
  [elapsed-time {:keys [delay duration]}]
  (/ (- elapsed-time delay) duration))

(defn lerp
  [start end progress]
  (+ start (* progress (- end start))))
