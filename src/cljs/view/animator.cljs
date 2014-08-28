(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan timeout close!]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))


;; https://github.com/jxa/rain/blob/master/src/cljs/rain/async.cljs:
(defn timer-chan
  "create a channel which emits a message every delay milliseconds
   if the optional stop parameter is provided, any value written
   to stop will terminate the timer"
  ([delay msg]
     (timer-chan delay msg (chan)))
  ([delay msg stop start]
     (let [out (chan)]
       (go-loop [running? true]
                (let [t (timeout delay)
                      [val ch] (alts! [stop t start])]
                  (when running?
                    (>! out msg))
                  (condp = ch
                    stop (recur false)
                    t (recur running?)
                    start (recur true))))
       out)))


(defn animator
  [cursor owner {:keys [stop? update index] :as opts}]
  (reify
    om/IInitState
    (init-state
     [this]
     {:stop-timer (chan)
      :start-timer (chan)})

    om/IWillMount
    (will-mount
     [_]
     (let [timer (timer-chan 10 :tick
                                (om/get-state owner :stop-timer)
                                (om/get-state owner :start-timer))]

       (go-loop []
                (<! timer)
                (let [time (.now (.-performance js/window))
                      elapsed-time (- time (om/get-state owner :start-time))]
                  (om/set-state! owner :elapsed-time elapsed-time))
                (recur))))

    om/IRenderState
    (render-state
     [this {:keys [elapsed-time stop-timer start-timer] :as state}]
     (when (zero? elapsed-time)
       (put! start-timer :start))
     (let [animation cursor]
       (when animation
         #_(println "animation:" index animation elapsed-time)
         (if (stop? elapsed-time animation)
           (do
             (println "stop" index "at" elapsed-time)
             (put! stop-timer :stop))
           (update elapsed-time (om/get-node owner "my-canvas") animation))))

     (dom/canvas #js {:ref "my-canvas" :id (str "animator-canvas-" index)
                      :style #js {:position "absolute" :left "0px" :top "0px"
                                  :width "800px" :height "400px"
                                  :z-index (* 2 (inc index))}
                      :width "800px" :height "400px"}))))
