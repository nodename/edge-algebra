(ns view.canvas)

(defn init-canvas
  "Initialize a canvas and return it."
  [parent z-index]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "width" (.-innerWidth js/window))
    (.setAttribute canvas "height" (.-innerHeight js/window))
    (.setAttribute canvas "style" (str "z-index:" z-index
                                       "; position:absolute; left:0px; top:0px;"))
    (.appendChild parent canvas)))

(def canvas-state (atom {:depth 1}))

(defn get-canvas
  [parent]
  (let [depth (get-in @canvas-state [:depth])]
    (swap! canvas-state update-in [:depth] inc)
    (println "DEPTH:" depth)
    (init-canvas parent depth)))
