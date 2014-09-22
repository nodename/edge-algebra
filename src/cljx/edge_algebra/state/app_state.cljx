(ns edge-algebra.state.app-state)

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


(defn get-val
  "Get the current value of edge. This gives us the up-to-date
  :next and :data fields."
  [edge]
  (get-in @app-state [:edge-records (:edge-record edge) :edges (:r edge) (:f edge)]))
