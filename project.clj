(defproject synthread "1.3.0"
  :description "Syntax Threading library"
  :url "http://github.com/myguidingstar/synthread/"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3126"]]
  :min-lein-version "2.0.0"
  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.6.0"]
                             [lein-cljsbuild "1.0.5"]
                             [lein-figwheel "0.2.5"]
                             [lein-pdo "0.1.1"]]
                   :cljx {:builds [{:source-paths ["src"]
                                    :output-path "src"
                                    :rules :clj}

                                   {:source-paths ["src"]
                                    :output-path "src"
                                    :rules :cljs}
                                   {:source-paths ["test"]
                                    :output-path "test"
                                    :rules :clj}

                                   {:source-paths ["test"]
                                    :output-path "test"
                                    :rules :cljs}]}
                   :cljsbuild {:builds {:test
                                        {:source-paths ["src" "test"]
                                         :notify-command ["nodejs" "target/simple.js"]
                                         :compiler {:main test-runner
                                                    :output-to "target/simple.js"
                                                    :output-dir "target/out"
                                                    :target :nodejs
                                                    :optimizations :none}}}}}})
