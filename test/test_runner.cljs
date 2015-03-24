(ns test-runner
  (:require [clojure.string :as str]
            [cljs.test :as test :refer-macros [run-tests] :refer [report]]
            [lonocloud.synthread.test]))

(enable-console-print!)

(defmethod report [::test/default :summary] [m]
  (println "\nRan " (:test m) " tests containing")
  (println (+ (:pass m) (:fail m) (:error m)) " assertions.")
  (println (:fail m) " failures, " (:error m) " errors."))

(defn run []
  (test/run-tests
   (test/empty-env ::test/default)
   'lonocloud.synthread.test))
