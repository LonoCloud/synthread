(ns lonocloud.synthread
  (:refer-clojure :exclude [defn defmacro])
  (#?(:clj :require :cljs :require-macros)
     [lonocloud.synthread.impl :refer [defn defmacro decloak]]))

#?(:clj
   (defn replace-content
     [o n]
     (condp instance? o
       (type n)                           (if (instance? clojure.lang.IObj o)
                                            (with-meta n (meta o))
                                            n)
       clojure.lang.IMapEntry             (vec n)
       clojure.lang.IRecord               (with-meta
                                            (merge o (if (map? n) n
                                                         (into {} (map vec n))))
                                            (meta o))
       clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
       clojure.lang.IPersistentMap        (into (empty o) (map vec n))

       clojure.lang.ISeq                  (with-meta (doall n) (meta o))
       clojure.lang.IPersistentCollection (into (empty o) n)

       clojure.lang.IObj                  (with-meta n (meta o))
       n))

   :cljs
   (defn replace-content
     [o n]
     (cond
      (satisfies? IMapEntry o)      (vec n)
      (satisfies? IRecord o)        (with-meta (merge o (if (map? n) n
                                                            (into {} (map vec n))))
                                      (meta o))
      (satisfies? IList o)          (with-meta (apply list n) (meta o))
      (satisfies? IMap o)           (into (empty o) (map vec n))

      (satisfies? ISeq o)           (with-meta (doall n) (meta o))
      (satisfies? ICollection o)    (into (empty o) n)

      :else n)))

(defn map-or-identity [f x]
  (if (coll? x)
    (map f x)
    x))

(defn prewalk [f form]
  ;; (prn :form form)
  (replace-content form (map-or-identity (partial prewalk f) (f form))))

(defn postwalk [f form]
  (f (replace-content form (map-or-identity (partial postwalk f) form))))

;; Section 1: macros that do not update the topic.
;;            Generally control flow macros.
(defmacro ^:cloaked do
  "Thread x through body. Semantically identical to -> with the
  extra feature that the symbol <> is bound to the new value of x
  between each form.

  Note that the old marking constraint has been removed as it was
  unduly restrictive."
  [x & body]
  (if (empty? body)
    x
    `(let [~'<> ~x]
       (__do (-> ~'<> ~(first body))
             ~@(rest body)))))

(defmacro ^:cloaked last
    "EXPERIMENTAL Thread the last element of x through body.
  (->/last [1 2 3] inc -) ;; returns [1 2 -4]"
    [x & body]
    `(let [x# ~x]
       (replace-content x# (concat (drop-last 1 x#)
                                   [(__do (last x#) ~@body)]))))

(defn ^:cloaked apply
  "Apply f to x and args."
  [x & f-args]
  (let [[f & args] (concat (drop-last f-args) (last f-args))]
    (apply f x args)))

(defn reset
  "Replace x with y."
  [x y] y)

;; Uncloak all our forms now that defining has completed.
(decloak do)
(decloak last)
(decloak apply)
