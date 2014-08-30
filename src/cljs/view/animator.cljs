(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan timeout close!]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))


(defn clock
  "Create a channel which emits the current time every delay milliseconds.
  Any value written to start/stop will start/stop the messages."
  [delay]
  (let [start (chan)
        stop (chan)
        out (chan)]
    (go-loop [running? false]
             (let [t (timeout delay)
                   [_ ch] (alts! [stop t start])]
               (when running?
                  (let [now (.now (.-performance js/window))]
                    (>! out now)))
               (condp = ch
                 stop (recur false)
                 t (recur running?)
                 start (recur true))))
    [out start stop]))


(defn animator
  [cursor owner {:keys [stop? update index] :as opts}]
  (reify
    om/IInitState
    (init-state
     [this]
     {:clock (clock 10)})
    ;; :start-time and :elapsed-time are injected


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
     [this {:keys [elapsed-time clock] :as state}]
     (let [[_ start stop] clock]
       (when (zero? elapsed-time)
         (put! start :start))
       (let [animation (nth cursor index)]
         (when animation
           (println "animation:" index animation elapsed-time)
           (if (stop? elapsed-time animation)
             (do
               (println "stop" index "at" elapsed-time)
               (put! stop :stop)) ;; will want to stop only when all anims are done
             (let [canvas (. js/document (getElementById (str "animator-canvas-" index)))]
               (when canvas ;; who knows exactly when it mounts
                 (update elapsed-time canvas animation))))))

       (dom/div #js {}
                (dom/canvas #js {:id (str "animator-canvas-" index)
                                 :style #js {:position "absolute" :left "0px" :top "0px"
                                             :width "800px" :height "400px"
                                             :z-index (* 2 (inc index))}
                                 :width "800px" :height "400px"}))))))

