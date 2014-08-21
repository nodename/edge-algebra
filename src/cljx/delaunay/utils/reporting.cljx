(ns delaunay.utils.reporting
  (:require [delaunay.div-conq :as dq]
            [#+clj clojure.core.async #+cljs cljs.core.async :refer [put!]]))


#+clj
(defn get-fn-name
  "Return the symbol that was used to define the function."
  [f]
  (-> (str f)
      (clojure.string/replace #".*\$" "")
      (clojure.string/replace #"\@.*$" "")
      (clojure.string/replace "_BANG_" "!")
      (clojure.string/replace "_QMARK_" "?")
      (clojure.string/replace \_ \-)
      symbol))

;; In ClojureScript (str f) gives the entire source code of the function,
;; and we're not going to be able to use the symbol, so make it a keyword:
#+cljs
(defn get-fn-name
  [f]
  (-> (str f)
      (.replace (js/RegExp. "\n" "gm"), "") ;; remove all newlines
      (.replace #"^function " "")
      (.replace #"\(.*" "") ;; remove open-paren and everything after it
      (.replace "_BANG_" "!")
      (.replace "_QMARK_" "?")
      (.replace \_ \-)
      keyword))


(defn wrap-with-name-and-args-reporting
  "Return a function which will put the symbol
  and current args of f onto ch before invoking f."
  [ch f]
  (fn [& args]
    (put! ch (vec (concat [(get-fn-name f)] args)))
    (apply f args)))


(defn with-reporting
  [ch f & [args]]
  (with-redefs [dq/make-d-edge! (wrap-with-name-and-args-reporting ch dq/make-d-edge!)
                dq/delete-edge! (wrap-with-name-and-args-reporting ch dq/delete-edge!)
                dq/in-circle? (wrap-with-name-and-args-reporting ch dq/in-circle?)]
    (f args)))
