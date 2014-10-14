(ns view.clock
  (:require [cljs.core.async :refer [put! <! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn clock
  "Return a channel 'out' which emits the current time every interval milliseconds,
  and two control channels 'start' and 'stop'.
  Any value written to start/stop will start/stop the messages.
  The :run-at-start named argument (default true) specifies whether or not
  the clock will start running immediately without waiting for the first start message."
  [interval & {:keys [run-at-start]
               :or {run-at-start true}}]
  (let [start (chan)
        stop (chan)
        out (chan)]
    (go-loop [running? run-at-start]
             (when running?
                  (let [now (.now (.-performance js/window))]
                    (>! out now)))

             (let [t (timeout interval)
                   [_ ch] (alts! [stop t start])]
               (condp = ch
                 stop (recur false)
                 t (recur running?)
                 start (recur true))))
    [out start stop]))
