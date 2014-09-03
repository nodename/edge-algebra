(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan timeout]]
            [view.clock :refer [clock]]
            [delaunay.utils.circle :refer [center-and-radius]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))


;;;; Animations  ;;;;
;;
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
          :duration 750
          :delay (* 650 index)}))

(defn make-animations
  [circles]
  (vec (map make-animation circles (range))))
;;
;;;;  Animations  ;;;;



(defn canvas-id
  [index]
  (str "animator-canvas-" index))

(defn get-canvas
  [index]
  (. js/document (getElementById (canvas-id index))))

(defn canvas-props
  [index]
  #js {:id (canvas-id index)
       :style #js {:position "absolute" :left "0px" :top "0px"
                   :width "800px" :height "400px"
                   :z-index (* 2 (inc index))}
       :width "800px" :height "400px"})


(defn animator
  [circles owner {:keys [update] :as opts}]
  (reify
    om/IInitState
    (init-state
     [this]
     {:clock (clock 10)})
   ;; :start-time must be injected

    om/IWillMount
    (will-mount
     [_]
     (let [[clock] (om/get-state owner :clock)]
       (go-loop []
                (let [now (<! clock)
                      elapsed-time (- now (om/get-state owner :start-time))]
                  (om/set-state! owner :elapsed-time elapsed-time))
                (recur))))

    om/IRenderState
    (render-state
     [_ {:keys [elapsed-time] :as state}]
     (let [animations (make-animations (om/value circles))]
       (doseq [index (range (count animations))]
         (let [animation (nth animations index)
               canvas (get-canvas index)]
           (when canvas ;; who knows exactly when it mounts
             (update elapsed-time canvas animation))))

     (apply dom/div #js {}
            (map (fn [index] (dom/canvas (canvas-props index)))
                 (range (count animations))))))))


