(set-env!
 :source-paths   #{"src" "test"}
 :resource-paths #{"html"}
 :dependencies '[[adzerk/boot-cljs "0.0-2814-3" :scope "test"]
                 [adzerk/boot-test "1.0.4" :scope "test"]
                 [deraen/boot-cljx "0.2.2"]
                 [com.cemerick/piggieback "0.1.5"]
                 [adzerk/boot-reload "0.2.6" :scope "test"]
                 [adzerk/bootlaces          "0.1.11"    :scope "test"]
                 [org.clojure/clojurescript "0.0-3123"  :scope "test"]])

(require
 '[adzerk.bootlaces :refer [bootlaces! build-jar push-release]]
 '[adzerk.boot-cljs      :refer [cljs]]
 '[deraen.boot-cljx      :refer [cljx]]
 '[adzerk.boot-reload    :refer [reload]]
 '[adzerk.boot-test :refer [test]])

(def +version+ "1.3.0")

(bootlaces! +version+)

(task-options!
 pom  {:project     'synthread
       :version     +version+
       :description "Syntax Threading library"
       :url         "http://github.com/lonocloud/synthread/"
       :scm         {:url "https://github.com/lonocloud/synthread"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask run-cljs-test
  "Run cljs tests"
  []
  (fn middleware [next-handler]
    (fn handler [fileset]
      (sh "node" "target/script.js")
      (-> fileset next-handler))))

(deftask dev []
  (comp (watch)
        (speak)
        (reload)
        (cljx)
        (test)
        (cljs :main 'test-runner
              :output-to "script.js"
              :asset-path "target/out"
              :target :nodejs
              :optimizations :none)
        (run-cljs-test)))

(deftask release []
  (comp (cljx)
        (build-jar)
        (push-release)))
