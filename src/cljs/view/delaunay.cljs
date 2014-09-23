(ns view.delaunay
  (:require [delaunay.div-conq :as dq
             :refer [pt delaunay]]
            [edge-algebra.state.app-mutators
             :refer [wrap-with-undo
                     wrap-with-add-circle
                     wrap-with-clear-circles
                     wrap-with-add-message
                     replace-with-add-message
                     wrap-with-name-and-args-reporting
                     wrap-with-clear-messages
                     reset-state!]]))


(defn decorate
  [f]
  (fn [& args]
    (with-redefs [edge-algebra.core/splice! (wrap-with-name-and-args-reporting
                                             edge-algebra.core/splice!)
                  dq/make-d-edge! (wrap-with-clear-messages
                                   (wrap-with-clear-circles
                                    (wrap-with-undo
                                     (wrap-with-name-and-args-reporting
                                      dq/make-d-edge!))))
                  dq/delete-edge! (wrap-with-clear-messages
                                   (wrap-with-clear-circles
                                    (wrap-with-undo
                                     (wrap-with-name-and-args-reporting
                                      dq/delete-edge!))))
                  dq/in-circle? (wrap-with-add-circle
                                 (wrap-with-name-and-args-reporting
                                  dq/in-circle?))
                  println (replace-with-add-message
                           println)]
      (apply f args))))

(def deco-delaunay (decorate delaunay))


(defn make-sites
  [w h how-many]
  (vec (for [_ (range how-many)]
         (pt (rand-int w) (rand-int h)))))


(defn run-delaunay
  []
  ((wrap-with-undo reset-state!))
  (let [sites (make-sites 800 400 40)]
    (println sites)
    (deco-delaunay sites)))
