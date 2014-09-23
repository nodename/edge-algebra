(ns utils.spawn
  (:require
   #+clj [clojure.core.async
          :refer [>! take! chan go go-loop alts!]]
   #+cljs [cljs.core.async
                    :refer [>! take! chan alts!]])
  #+cljs (:require-macros [cljs.core.async.macros
                           :refer [go go-loop]]))

(defn go-fn
  [f]
  (fn [& args]
    (let [ch (chan)]
      (go
       (let [result (apply f args)]
         (println "args:" args "result:" result)
         (>! ch result)))
      ch)))

(defn spawn
  "Return a function which will run f in a go-block
  and put f's return value into the atom a when it is done."
  [f a]
  (fn [& args]
    (let [ch (apply (go-fn f) args)]
      (take! ch #(reset! a %)))))


(defn vec-chan
  "Get one value from each of chs and put a vector of those values
  onto a new channel. Return the new channel. (Assumes each of chs
  emits exactly one value.)"
  [& chs]
  (let [values []
        out (chan)]
    (go (dotimes [_ (count chs)]
          (let [[val ch] (alts! chs)]
            (assoc values (.indexOf chs ch) val)))
        (>! out values))
    out))
