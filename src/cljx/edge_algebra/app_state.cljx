(ns edge-algebra.app-state
  (:require [utils.reporting :refer [get-fn-name]]
     #+cljs [om.core :as om :include-macros true]))

(def initial-state {:edge-records []
                    :circles []
                    :messages []})

(def app-state (atom initial-state))

#+cljs
(def cursor (atom nil))

#+cljs
(defn set-cursor!
  [c]
  (reset! cursor c))

(defn next-er-index
  []
  (count (:edge-records @app-state)))

(defn get-edge-record
  [edge-or-node]
  (let [er-index (:edge-record edge-or-node)]
    ((:edge-records @app-state) er-index)))

;; Mutators:


#+cljs
(defn add-to-undo!
  "Add a marker transaction to the undo list for the current cursor state."
  []
  (om/transact! @cursor [] (constantly @app-state) :add-to-undo))


#+cljs
(defn wrap-with-undo
  "Return a function which will add the current app state to the undo list
  after invoking f."
  [f]
  (fn [& args]
    (let [val (apply f args)]
      (add-to-undo!)
      val)))

(defn update!
  [path value]
  #+clj (swap! app-state assoc-in path value)
  #+cljs (om/transact! @cursor path (constantly value)))

(defn reset-state!
  []
  (update! [] initial-state))


(defn add-edge-record!
  [er]
  #+clj (swap! app-state update-in [:edge-records] conj er)
  #+cljs (om/transact! @cursor [:edge-records] #(conj % er)))

(defn add-circle!
  [c]
  #+clj (swap! app-state update-in [:circles] conj c)
  #+cljs (om/transact! @cursor [:circles] #(conj % c)))

(defn clear-circles!
  []
  (update! [:circles] []))

(defn wrap-with-add-circle
  "Return a function that will add the args of f to :circles
  before/after invoking f."
  [when f]
  (fn [& args]
    (condp = when
      :before (do
                (add-circle! args)
                (apply f args))
      :after (let [val (apply f args)]
               (add-circle! args)
               val))))

(defn wrap-with-clear-circles
  "Return a function that will reset :circles before/after invoking f."
  [when f]
  (fn [& args]
    (condp = when
      :before (do
                (clear-circles!)
                (apply f args))
      :after (let [val (apply f args)]
               (clear-circles!)
               val))))

(defn add-message!
  [args]
  #+clj (swap! app-state update-in [:messages] conj args)
  #+cljs (om/transact! @cursor [:messages] #(conj % args)))

(defn clear-messages!
  []
  (update! [:messages] []))

(defn wrap-with-add-message
  "Return a function that will add the args of f to :messages
  after invoking f."
  [f]
  (fn [& args]
    (let [val (apply f args)]
      (add-message! args)
      val)))

(defn replace-with-add-message
  "Return a function that will add the args of f to :messages
  instead of invoking f."
  [f]
  (fn [& args]
    (add-message! args)))

(defn wrap-with-name-and-args-reporting
  "Return a function that will add the symbol
  and current args of f to :messages before invoking f."
  [f]
  (fn [& args]
    (add-message! (vec (concat [(get-fn-name f)] args)))
    (apply f args)))

(defn wrap-with-clear-messages
  "Return a function that will reset :messages after invoking f."
  [f]
  (fn [& args]
    (let [val (apply f args)]
      (clear-messages!)
      val)))


(defn remove-edge-record!
  "Mark edge's edge record as deleted. We don't really delete it
  because edge records are referred to by their indices in :edge-records."
  [edge]
  (let [er-index (:edge-record edge)
        path [:edge-records er-index :deleted]]
    (update! path true)))


(defn set-data!
  "Set edge's data. Return the updated edge."
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)
        path [:edge-records er-index :edges r f :data]]
    (update! path data)
    (get-in @app-state (vec (butlast path)))))


(defn set-next!
  "Set edge's next. Return the updated edge."
  [edge next-edge]
  (let [next {:r (:r next-edge)
              :f (:f next-edge)
              :edge-record (:edge-record next-edge)}
        er-index (:edge-record edge)
        r (:r edge)
        f (:f edge)
        path [:edge-records er-index :edges r f :next]]
    (update! path next)
    (get-in @app-state (vec (butlast path)))))
