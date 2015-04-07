(require 'cljs.repl)
(require 'cljs.closure)
(require 'cljs.repl.rhino)

(cljs.closure/build "src"
                    {:main 'hello-world.core
                     :output-to "out/main.js"
                     :verbose true})

(cljs.repl/repl (cljs.repl.rhino/repl-env)
                :watch "src"
                :output-dir "out")
