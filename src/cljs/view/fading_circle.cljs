(ns view.fading-circle)

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

(defn fading-circle-update
  "Draw the circle and the dot, fading"
  [elapsed-time canvas {:keys [center radius dot line-width scale color delay] :as opts}]
  (let [context (.getContext canvas "2d")
        a (alpha elapsed-time opts)]
    (when (>= elapsed-time delay)
      (.clearRect context 0 0 (.-width canvas) (.-height canvas))
      (draw-circle context center radius line-width scale
                   (merge color {:a a}))
      (draw-circle context dot (/ 6 scale) line-width scale
                   (merge color {:a a})))))
