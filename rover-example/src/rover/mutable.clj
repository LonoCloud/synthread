(ns rover.mutable)

;;; mutable example

(defn new-rover []
  {:battery (atom {:running true})})

(defn send-message [{:keys [to body]}]
  (println "Launches missiles!!")
  ;; etc...
  )

(defn shutdown [battery]
  (println "Also launches missiles!")
  ;; etc...
  )

(defn test-shutdown [battery]
  (println "Shutting down the battery")
  (swap! battery assoc :running false))

(defn test-send-message [{:keys [to body]}]
  (println "Sending to" to body))

(defn update-rover
  [rover {:keys [temp] :as forecast}]

  (when (< temp -35.3)
    (shutdown (:battery rover))
    (send-message {:to :nasa
                   :body "temp too low"}))
  (send-message {:to :nasa
                 :body forecast}))

(defn test-example []
  (with-redefs [shutdown test-shutdown
                send-message test-send-message]
    (let [rover (new-rover)]
      (update-rover rover {:temp -20})
      (update-rover rover {:temp -50})
      rover)))

;; You probably shouldn't run this
(defn real-example []
  (let [rover (new-rover)]
    (update-rover rover {:temp -20})
    (update-rover rover {:temp -50})
    rover))
