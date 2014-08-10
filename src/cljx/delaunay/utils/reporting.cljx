(ns delaunay.utils.reporting)

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
  [f]
  (fn [& args]
    (apply prn (get-fn-init-sym f) args)
    (apply f args)))

