(defproject edge-algebra "0.1.0-SNAPSHOT"
  :description "An implementation of
  Guibas, L. and J. Stolfi. 1985. \"Primitives for the Manipulation of
  General Subdivisions and the Computation of Voronoi Diagrams,\"
  ACM Transactions on Graphics 4(2):74-123."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2280"]
                 [org.clojure/core.async "0.1.319.0-6b1aca-alpha"]
                ; we're keeping a local copy of this: [thi.ng/geom "0.3.0-SNAPSHOT"]
                 ]
      :min-lein-version "2.0.0"
      :source-paths ["src/clj" "target/generated/clj"]

      :plugins [[lein-cljsbuild "0.3.0"]
                [lein-simpleton "1.1.0"]
                [com.keminglabs/cljx "0.4.0"]]

      :cljx {:builds [{:source-paths ["src/cljx"]
                       :output-path "target/generated/clj"
                       :rules :clj}

                      {:source-paths ["src/cljx"]
                       :output-path "target/generated/cljs"
                       :rules :cljs}]}

      :hooks [cljx.hooks]

      :cljsbuild {:builds [{:source-paths ["src/cljs" "target/generated/cljs"]
                            :compiler {:output-to "resources/public/build/deps.js"
                                       :output-dir "resources/public/build"
                                     ;  :pretty-print true
                                     ;  :optimizations :advanced}}]})
                                     ;  :optimizations :simple}}]})
                                       :optimizations :none}}]})
