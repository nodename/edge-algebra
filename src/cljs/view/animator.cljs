(ns view.animator
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [delaunay.utils.circle :refer [center-and-radius]]
            [cljs.core.async :refer [put! <! chan timeout close!]]
            [view.canvas :refer [get-canvas]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; A generic one-shot time-based animation component.


;; https://github.com/jxa/rain/blob/master/src/cljs/rain/async.cljs:
(defn timer-chan
  "create a channel which emits a message every delay milliseconds
   if the optional stop parameter is provided, any value written
   to stop will terminate the timer"
  ([delay msg]
     (timer-chan delay msg (chan)))
  ([delay msg stop]
     (let [out (chan)]
       (go-loop []
                (let [[val ch] (alts! [stop (timeout delay)])]
                  (when-not (= ch stop)
                    (>! out msg)
                      (recur))))
       out)))


(defn animator
  [cursor owner {:keys [stop? update index] :as opts}]
  (reify
    om/IInitState
    (init-state
     [this]
     {:stop-timer (chan)
      })

    om/IWillMount

    (will-mount
     [_]
     (let [timer (timer-chan 10 :tick (om/get-state owner :stop-timer))]
       (go-loop []
                (<! timer)
                (let [time (.now (.-performance js/window))
                      elapsed-time (- time (om/get-state owner :start-time))]
                  (om/set-state! owner :elapsed-time elapsed-time))
                (recur))))

    om/IRenderState
    (render-state
     [this {:keys [elapsed-time stop-timer] :as state}]
     (let [animation cursor]
       (when animation
         (println "animation:" index animation elapsed-time)
         (if (stop? elapsed-time animation)
           (do
             (println "stop" index "at" elapsed-time)
             (put! stop-timer :stop))
           (update elapsed-time (om/get-node owner "my-canvas") animation))))

     (dom/canvas #js {:ref "my-canvas" :id (str "animator-canvas-" index)
                      :z-index (* 2 (inc index))
                      :position "absolute" :left "0px" :top "0px"}))))
