(ns rover.monad
  (:require [monads.core :as m]))

;;; pure functional example, using Jim Duey's protocol monads

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

(defn update-rover [{:keys [temp] :as forecast}]
  (m/seq
   [(if (< temp -35.3)
      (m/seq [(m/update-val
               :outbox conj
               {:to :nasa
                :body "temp too low"})
              (m/update-val :battery shutdown)])
      (m/update-state identity))
    (m/update-val :outbox conj
                  {:to :nasa
                   :body forecast})]))

(defn apply-monad
  "Takes an input state and a State monadic value. Returns the final state"
  [rover mv]
  (second (mv rover)))

(defn example [do-side-effects]
  (let [rover (do-side-effects (new-rover))
        rover (do-side-effects (apply-monad rover (update-rover {:temp -20})))
        rover (do-side-effects (apply-monad rover (update-rover {:temp -50})))]
        ;;                      ^ phase 2          ^ phase 1
    rover))

(defn test-example []
  (example test-side-effects))

;; Implementing real-side-effects is left as an exercise for the
;; reader.
#_(defn real-example []
    (example real-side-effects))
