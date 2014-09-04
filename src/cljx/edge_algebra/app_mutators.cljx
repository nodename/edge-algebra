(ns edge-algebra.app-mutators
  (:require [edge-algebra.app-state :refer [app-state initial-state
                                            #+cljs cursor
                                            #+cljs set-cursor!]]
            [utils.reporting :refer [get-fn-name]]
            [edge-algebra.cheat-codes :refer [edge-info show-all-edge-records]]
     #+cljs [om.core :as om :include-macros true]))



;; Mutators:


#+cljs
(defn add-to-undo!
  "Add a marker transaction to the undo list for the current cursor state."
  []
  (om/transact! @cursor [] identity :add-to-undo))


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
  [& _]
  (update! [:circles] []))

(defn wrap-before
  [f aux]
  (fn [& args]
    (do
      (aux args)
      (apply f args))))

(defn wrap-after
  [f aux]
  (fn [& args]
    (let [val (apply f args)]
      (aux args)
      val)))

(defn wrap-with-add-circle
  "Return a function that will add the args of f to :circles
  before invoking f."
  [f]
  (wrap-before f add-circle!))

(defn wrap-with-clear-circles
  "Return a function that will reset :circles after invoking f."
  [f]
  (wrap-after f clear-circles!))

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
  (wrap-after f add-message!))

(defn replace-with-add-message
  "Return a function that will add the args of f to :messages
  instead of invoking f."
  [f]
  (fn [& args]
    (add-message! args)))

(defn wrap-with-name-and-args-reporting
  "Return a function that will add the name
  and current args of f to :messages before invoking f,
  and the name and return value after."
  [f]
  (fn [& args]
    (let [fn-name (get-fn-name f)]
      (add-message! (vec (concat [fn-name] args)))
      (let [val (apply f args)]
       #_(add-message! (vec (concat [fn-name] args [val]))) ;; !! this causes an error!
        val))))


(defn wrap-with-add-circle-and-reporting
  "Return a function that will add the args of f to :circles
  before invoking f."
  [f]
  (fn [& args]
    (do
      (add-circle! args)

      (apply f args))))


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


(defn set-sym-data!
  "Set sym edge's data. Return the calling edge."
  [edge data]
  (let [er-index (:edge-record edge)
        r (:r edge)
        sym-r (mod (+ 2 (:r edge)) 4)
        f (:f edge)
        path [:edge-records er-index :edges sym-r f :data]]
    (update! path data)
    (get-in @app-state [:edge-records er-index :edges r f])))


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
        (println )
    (update! path next)
     (get-in @app-state (vec (butlast path)))))
