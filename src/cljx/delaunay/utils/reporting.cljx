(ns delaunay.utils.reporting
  (:require [clojure.core.async :refer [>!!]]))

(defn get-fn-init-sym
  "Return the symbol that was used to define the function."
  [f]
  (let [trim (fn [s] (re-find #"(?<=\$).*(?=@)" s))]
    (-> (str f)
        trim
        (clojure.string/replace "_BANG_" "!")
        (clojure.string/replace "_QMARK_" "?")
        (clojure.string/replace \_ \-)
        symbol)))


(defn wrap-with-name-and-args-reporting
"Return a function which will put the symbol and current args of f onto ch before running f."
  [ch f]
  (fn [& args]
    (>!! ch (vec (concat [(get-fn-init-sym f)] args)))
    (apply f args)))

