(ns utils.reporting)


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


