(ns lonocloud.synthread.monad
  (:require [monads.core :as m]
            [lonocloud.synthread :as ->]))

(defn update-state [mv f & args]
  (m/bind mv (fn [v]
               (m/do-result mv (apply f v args)))))

(defmacro ->result [v mv-template]
  `(m/do-result ~mv-template ~v))

(defmacro ->$as [mv binding & body]
  `(let [mv# ~mv]
     (m/bind mv#
             (fn [v#]
               (-> v#
                 (->/as ~binding
                   (->result mv#)
                   ~@body))))))

(defmacro ->$aside [mv binding & body]
  `(->$as ~mv ~binding (doto (do ~@body))))

;; is this a thing?
#_(defn big-lift [mv f g]
    (m/bind mv (fn [sub-state]
                 (g (m/do-result mv (f sub-state))))))

(defmacro call-bits [mv get-set & body]
  `(let [mv# ~mv
         template# (m/do-result mv# nil)
         get-set# ~get-set]
     (m/bind mv# (fn [state#]
                   (-> (m/do-result template# (get-set# state#))
                     ~@body
                     (m/bind (fn [sub-state#]
                               (m/do-result template# (get-set# state# sub-state#)))))))))

(defn first+
  ([state] (first state))
  ([state sub-state] (cons sub-state (rest state))))

(defmacro ->$first [mv & body]
  `(call-bits ~mv first+ ~@body))

(defn get+
  ([k state] (get state k))
  ([k state sub-state] (assoc state k sub-state)))

(defmacro ->$assoc [mv & pairs]
  `(-> ~mv
     ~@(for [[k v] (partition 2 pairs)]
         `(call-bits (partial get+ ~k) ~v))))

;;;  -- the state-msgs monad

(declare state-msgs)

(defrecord StateMsgs [state msgs]
  m/Monad
  (do-result [_ s] (state-msgs s))
  (bind [mv f]
    (let [new-mv (f (:state mv))]
      (StateMsgs. (:state new-mv)
                  (into (:msgs mv) (:msgs new-mv))))))

(defn state-msgs [state]
  (StateMsgs. state []))

(defn send [mv & msgs]
  (m/bind mv (fn [state] (StateMsgs. state (vec msgs)))))

(comment
  (-> (state-msgs {:a [1 2 3]})
    (->$assoc
     :a (->$first
         (update-state * 10)
         (->$as new-num
           (ask (str "hello " new-num)))))))
