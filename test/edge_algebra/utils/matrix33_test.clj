(ns edge-algebra.utils.matrix33-test
  (:require [clojure.test :refer :all]
            [delaunay.utils.matrix33 :refer [matrix33]]))

(deftest sanity
  (let [m (matrix33 2 4 6
                    3 1 9
                    12 9 3)]
    (is (= (.determinant m) 330.0))))
