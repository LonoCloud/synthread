(ns rover.synthread
  (:require [lonocloud.synthread :as ->]))

;;; pure functional example, using LonoCloud Synthread

(defn new-rover []
  {:battery {:running true}
   :outbox []})

(defn test-side-effects
  "This is the only impure function in this example. All side effects
  are encapsulated here."
  [{:keys [outbox battery] :as rover}]
  (-> rover
    (->/when (:shutting-down battery)
      (->/aside _ (println "Shutting down the battery"))
      (->/in [:battery]
        (dissoc :shutting-down)
        (assoc :running false)))
    (->/for [{:keys [to body]} outbox]
      (->/aside _ (println "Sending to" to body)))
    (assoc :outbox [])))

(defn shutdown [battery]
  (assoc battery :shutting-down true))

(defn update-rover
  [rover {:keys [temp] :as forecast}]
  (-> rover
    (->/when (< temp -35.3)
      (->/update
        :battery shutdown
        :outbox (conj {:to :nasa
                       :body "temp too low"})))
    (->/update :outbox (conj {:to :nasa
                              :body forecast}))))

(defn example [do-side-effects]
  (-> (new-rover)
    do-side-effects
    (update-rover {:temp -20})
    do-side-effects
    (update-rover {:temp -50})
    do-side-effects))

(defn test-example []
  (example test-side-effects))

;; Implementing real-side-effects is left as an exercise for the
;; reader.
#_(defn real-example []
    (example real-side-effects))
