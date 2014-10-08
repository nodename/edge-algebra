(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan timeout >!]]
            [view.clock :refer [clock]]
            [edge-algebra.record :refer [get-e0 get-edge]]
            [edge-algebra.state.app-mutators :refer [clear-current-edge-record!]]
            [view.animation.drawing-line :refer [growing-line-update
                                                 shrinking-line-update]]
            [delaunay.utils.circle :refer [center-and-radius]]
            [view.animation.fading-circle :refer [fading-circle-update]])
  (:require-macros [cljs.core.async.macros :refer [go]]))


;;;; Animations  ;;;;
;;
(def palette (cycle [{:r 138 :g 155 :b 15}
                    {:r 0 :g 160 :b 176}
                    {:r 204 :g 51 :b 63}
                    {:r 235 :g 104 :b 65}
                    {:r 237 :g 201 :b 81}]))

(defn make-delay
  [index]
  (* 500 index))

(defn animate-circle
  [in-circle-args index]
  (merge (apply center-and-radius (butlast in-circle-args))
         {:dot (last in-circle-args)
          :line-width 2
          :scale 1
          :color (nth palette index)
          :update fading-circle-update
          :duration 500
          :delay (make-delay index)}))

(defn animate-circles
  [circles]
  (vec (map animate-circle circles (range))))

(defn animate-line
  [edge-records {:keys [type er-index]} anim-index]
  (when type
    (let [update (condp = type
                   :adding growing-line-update
                   :removing shrinking-line-update)
          edge-record (edge-records er-index)
          p0 (:data (get-e0 edge-record))
          p1 (:data (get-edge edge-record 2 0))]
      (when (and p0 p1)
        {:start p0
         :end p1
         :line-width 2
         :scale 1
         :color {:r 255 #_71 :g 0 #_189 :b 254}
         :update update
         :duration 500
         :delay (make-delay anim-index)}))))
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
  [cursor owner]
  (reify
    om/IInitState
    (init-state
     [this]
     {:clear-current-ch (chan)
      :clock (clock 10)})
   ;; :start-time must be injected

    om/IWillMount
    (will-mount
     [_]
     (println "will-mount")
     (let [[clock] (om/get-state owner :clock)
           clear-current-ch (om/get-state owner :clear-current-ch)]
       (go
        (loop []
                (let [now (<! clock)
                      elapsed-time (- now (om/get-state owner :start-time))]
                  (om/set-state! owner :elapsed-time elapsed-time))
                (recur)))
       (go
        (loop []
          (<! clear-current-ch)
          ;; this will update cursor state and cause another render! not good!
          #_(clear-current-edge-record!)
          (recur)))))

    om/IRenderState
    (render-state
     [_ {:keys [elapsed-time clear-current-ch] :as state}]
     (let [circle-animations (animate-circles (om/value (:circles cursor)))
           op-type (om/value (:type (:current-edge-record cursor)))
           _ (println "RS:" (count circle-animations) op-type)
           line-animation (animate-line
                           (om/value (:edge-records cursor))
                           (om/value (:current-edge-record cursor))
                           (count circle-animations))
           animations (if line-animation
                          (concat circle-animations [line-animation])
                          circle-animations)]

       (doseq [index (range (count animations))]
         (let [animation (nth animations index)
               canvas (get-canvas index)
               update (:update animation)]
           (when canvas ;; who knows exactly when it mounts
             (update elapsed-time canvas animation))))

       (when (= op-type :adding)
         (let [timeout-ch (timeout (make-delay (count animations)))]
           (go
            (<! timeout-ch)
            (>! clear-current-ch :clear))))

       (apply dom/div #js {}
              (map (fn [index] (dom/canvas (canvas-props index)))
                   (range (count animations))))))))


