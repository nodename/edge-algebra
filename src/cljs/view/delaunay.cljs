(ns view.delaunay
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan]]
            [delaunay.div-conq :as dq :refer [pt delaunay]]
            [edge-algebra.app-mutators :refer [wrap-with-undo
                                               wrap-with-add-circle
                                               wrap-with-clear-circles
                                               wrap-with-add-message
                                               replace-with-add-message
                                               wrap-with-name-and-args-reporting
                                               wrap-with-clear-messages
                                               reset-state!]]))


(defn with-decorations
  [f & [args]]
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
    (f args)))


(defn make-sites
  [w h how-many]
  (vec (for [_ (range how-many)]
         (pt (rand-int w) (rand-int h)))))

#_
(defn run-delaunay
  []
  (reset-state!)
  (let [a (pt 0 0)
        b (pt 0 1)
        c (pt 1 0)
        d (pt 2 0)
        e (pt 3 1)
        f (pt 4 0)]
    (with-decorations delaunay [a b c d e f])))

(defn run-delaunay
  []
  (reset-state!)
  (let [
       ; sites (make-sites 800 400 20)
        sites [(pt 401 98) (pt 292 325) (pt 652 164) (pt 456 370) (pt 383 191)
               (pt 338 379) (pt 477 26) (pt 518 370) (pt 375 71) (pt 527 280)
               (pt 204 386) (pt 280 295) (pt 384 166) (pt 719 16) (pt 81 341)
               (pt 394 364) (pt 183 71) (pt 12 371) (pt 200 326) (pt 541 314)]
       ; sites [(pt 357 111) (pt 720 46) (pt 149 307) (pt 149 284)]
       ; sites [(pt 41 66) (pt 370 328) (pt 664 82) (pt 774 53)]
        ]
    (println sites)
    (with-decorations delaunay sites)))
