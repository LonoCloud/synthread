(ns rover.functional)

;;; pure functional example, using just stock clojure

(defn new-rover []
  {:battery {:running true}
   :outbox []})

(defn test-side-effects
  "This is the only impure function in this example. All side effects
  are encapsulated here."
  [{:keys [outbox battery] :as rover}]
  (let [battery (if (:shutting-down battery)
                  (do
                    (println "Shutting down the battery")
                    (-> battery
                        (dissoc :shutting-down)
                        (assoc :running false)))
                  battery)]
    (doseq [{:keys [to body]} outbox]
      (println "Sending to" to body))
    (-> rover
        (assoc :battery battery)
        (assoc :outbox []))))

(defn shutdown [battery]
  (assoc battery :shutting-down true))

(defn update-rover
  [{:keys [outbox battery] :as rover}
   {:keys [temp] :as forecast}]
  (let [rover
        (if (< temp -35.3)
          (assoc rover
            :battery (shutdown battery)
            :outbox
            (conj outbox
                  {:to :nasa
                   :body "temp too low"}))
          rover)]
    (update-in rover [:outbox] conj
               {:to :nasa :body forecast})))

(defn example [do-side-effects]
  (let [rover (do-side-effects (new-rover))
        rover (do-side-effects (update-rover rover {:temp -20}))
        rover (do-side-effects (update-rover rover {:temp -50}))]
    rover))

(defn test-example []
  (example test-side-effects))

;; Implementing real-side-effects is left as an exercise for the
;; reader.
#_(defn real-example []
    (example real-side-effects))