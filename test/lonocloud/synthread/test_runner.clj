(ns lonocloud.synthread.test-runner
  (:require lonocloud.synthread.test
            clojure.test
            cljs.repl
            cljs.repl.rhino))

(defn forms-reader [forms]
  (let [forms-atom (atom forms)]
    (fn [_ done]
      (if (empty? @forms-atom)
        done
        (let [form (first @forms-atom)]
          (swap! forms-atom next)
          form)))))

(defn cljs-eval-forms* [repl-env forms]
  (cljs.repl/repl repl-env
                  :output-dir "target/cljs-out"
                  :read (forms-reader forms)
                  :quit-prompt (constantly false)
                  :prompt      (constantly false)
                  :print       (constantly false)))

(defmacro cljs-eval-forms [repl-env & forms]
  `(cljs-eval-forms* ~repl-env '~forms))

(defn -main [& argv]
  (println "\n== Clojure tests ==")
  (prn (select-keys (clojure.test/test-ns 'lonocloud.synthread.test) [:test :pass :fail :error]))

  (println "\n== ClojureScript tests ==")
  (cljs-eval-forms (cljs.repl.rhino/repl-env)

    (require 'lonocloud.synthread.test)
    (defmethod cljs.test/report [:cljs.test/default :summary] [m]
      (prn (select-keys m [:test :pass :fail :error])))
    (cljs.test/run-tests
     (cljs.test/empty-env :cljs.test/default)
     'lonocloud.synthread.test)))
