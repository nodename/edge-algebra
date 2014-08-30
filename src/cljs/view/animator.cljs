(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan timeout close!]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))


(defn clock
  "Create a channel which emits the current time every interval milliseconds.
  Any value written to start/stop will start/stop the messages."
  [interval]
  (let [start (chan)
        stop (chan)
        out (chan)]
    (go-loop [running? true]
             (let [t (timeout interval)
                   [_ ch] (alts! [stop t start])]
               (when running?
                  (let [now (.now (.-performance js/window))]
                    (>! out now)))
               (condp = ch
                 stop (recur false)
                 t (recur running?)
                 start (recur true))))
    [out start stop]))


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
  [animations owner {:keys [stop? update] :as opts}]
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
     (doseq [index (range (count animations))]
       (let [animation (nth animations index)]
         (when-not (stop? elapsed-time animation)
           (let [canvas (get-canvas index)]
             (when canvas ;; who knows exactly when it mounts
               (update elapsed-time canvas animation))))))

     (apply dom/div #js {}
            (map (fn [index] (dom/canvas (canvas-props index)))
                 (range (count animations)))))))

