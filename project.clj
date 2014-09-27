(defproject com.nodename/edge-algebra "0.1.0-SNAPSHOT"
  :description "An implementation of
  Guibas, L. and J. Stolfi. 1985. \"Primitives for the Manipulation of
  General Subdivisions and the Computation of Voronoi Diagrams,\"
  ACM Transactions on Graphics 4(2):74-123."
  :url "http://github.com/nodename/edge-algebra"
  :author "Alan Shaw"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2280"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [om "0.7.1"]
                ; we're keeping a local copy of this: [thi.ng/geom "0.3.0-SNAPSHOT"]
                 ]
  :min-lein-version "2.0.0"
  :source-paths ["src/clj" "target/generated/clj"]

  :main "delaunay.main"

  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-simpleton "1.1.0"]
            [com.keminglabs/cljx "0.4.0"]]

  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.4.0"]]}}
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]}

  :hooks [cljx.hooks]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "target/generated/cljs"]
                        :compiler {:output-to "resources/public/build/dev/edge_algebra.dev.js"
                                   :output-dir "resources/public/build/dev"
                                   :source-map true
                                   :optimizations :none}}

                       {:id "prod"
                        :source-paths ["src/cljs" "target/generated/cljs"]
                        :compiler {:output-to "resources/public/build/prod/edge_algebra.prod.js"
                                   :output-dir "resources/public/build/prod"
                                   :source-map "edge_algebra.prod.js.map"
                                   :optimizations :advanced}}]})
